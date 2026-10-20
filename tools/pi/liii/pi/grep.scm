;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi grep)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii list)
    (liii os)
    (liii path)
    (liii subprocess)
    (liii either)
    (liii pi bash)
    (liii pi read)
    (liii pi find)
    (liii pi truncate)
  ) ;import
  (export pi-grep-schema pi-execute-grep)
  (begin

    (define (%prop-desc type desc)
      (list (cons "type" type) (cons "description" desc))
    ) ;define

    (define (pi-grep-schema)
      (list (cons "type" "object")
        (cons "properties"
          (list (cons "pattern"
                  (%prop-desc "string" "Search pattern (regex or literal string)")
                ) ;cons
            (cons "path"
              (%prop-desc "string" "Directory or file to search (default: current directory)")
            ) ;cons
            (cons "glob"
              (%prop-desc "string"
                "Filter files by glob pattern, e.g. '*.ts' or '**/*.spec.ts'"
              ) ;%prop-desc
            ) ;cons
            (cons "ignoreCase"
              (%prop-desc "boolean" "Case-insensitive search (default: false)")
            ) ;cons
            (cons "literal"
              (%prop-desc "boolean"
                "Treat pattern as literal string instead of regex (default: false)"
              ) ;%prop-desc
            ) ;cons
            (cons "context"
              (%prop-desc "number"
                "Number of lines to show before and after each match (default: 0)"
              ) ;%prop-desc
            ) ;cons
            (cons "limit"
              (%prop-desc "number" "Maximum number of matches to return (default: 100)")
            ) ;cons
          ) ;list
        ) ;cons
        (cons "required" #("pattern"))
      ) ;list
    ) ;define

    (define (%format-grep-block file-lines rel-path line-num context)
      (let* ((total (length file-lines))
             (start (if (> context 0) (max 1 (- line-num context)) line-num))
             (end (if (> context 0) (min total (+ line-num context)) line-num))
            ) ;
        (let loop
          ((curr start) (acc '()) (any-trunc? #f))
          (if (> curr end)
            (values (reverse acc) any-trunc?)
            (let* ((line (list-ref file-lines (- curr 1)))
                   (is-match? (= curr line-num))
                   (sep (if is-match? ":" "-"))
                  ) ;
              (let-values (((trunc-text was-trunc?) (pi-truncate-line line 500)))
                (let ((entry (string-append rel-path sep (number->string curr) sep " " trunc-text)))
                  (loop (+ curr 1) (cons entry acc) (or any-trunc? was-trunc?))
                ) ;let
              ) ;let-values
            ) ;let*
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (%scan-file-literal file-lines rel-path pattern ignore-case? limit)
      (let ((target-pat (if ignore-case? (string-downcase pattern) pattern)))
        (let loop
          ((rest file-lines) (idx 1) (acc '()))
          (cond
           ((or (null? rest) (>= (length acc) limit)) (reverse acc))
           ((string-contains? (if ignore-case? (string-downcase (car rest)) (car rest))
              target-pat
            ) ;string-contains?
            (loop (cdr rest) (+ idx 1) (cons (list rel-path idx) acc))
           ) ;
           (else (loop (cdr rest) (+ idx 1) acc))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (%parse-grep-line line rel-path)
      (let ((parts (string-split line ":")))
        (cond
         ((and (>= (length parts) 4) (= (string-length (car parts)) 1))
          (let ((n (string->number (caddr parts))))
            (and n (list rel-path n))
          ) ;let
         ) ;
         ((>= (length parts) 3)
          (let ((n (string->number (cadr parts))))
            (and n (list rel-path n))
          ) ;let
         ) ;
         (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%scan-file-regex abs-path rel-path pattern ignore-case? timeout)
      (let* ((args (append (list (pi-find-grep-path) "-H" "-n" "-E")
                     (if ignore-case? (list "-i") '())
                     (list "--" pattern abs-path)
                   ) ;append
             ) ;args
             (res (run-either args :stdout 'capture :stderr 'discard :timeout timeout))
            ) ;
        (if (either-right? res)
          (let ((lines (string-split (to-right res) "\n")))
            (let loop
              ((lst lines) (acc '()))
              (if (null? lst)
                (reverse acc)
                (let ((parsed (%parse-grep-line (car lst) rel-path)))
                  (loop (cdr lst) (if parsed (cons parsed acc) acc))
                ) ;let
              ) ;if
            ) ;let
          ) ;let
          '()
        ) ;if
      ) ;let*
    ) ;define

    (define (%build-grep-notices limit-reached? limit truncated? lines-truncated?)
      (let ((notices
              (append
                (if limit-reached?
                  (list (string-append (number->string limit)
                          " matches limit reached. Use limit="
                          (number->string (* limit 2))
                          " for more, or refine pattern"
                        ) ;string-append
                  ) ;list
                  '()
                ) ;if
                (if truncated? (list "50.0KB limit reached") '())
                (if lines-truncated?
                  (list "Some lines truncated to 500 chars. Use read tool to see full lines")
                  '()
                ) ;if
              ) ;append
            ) ;notices
           ) ;
        (if (null? notices) "" (string-append "\n\n[" (string-join notices ". ") "]"))
      ) ;let
    ) ;define

    (define (%collect-grep-candidates abs-path is-dir? glob-pat timeout)
      (if (not is-dir?)
        (list (list abs-path (path-name (path abs-path))))
        (let* ((pat (or glob-pat "*"))
               (files (pi-execute-find (list (cons "path" abs-path) (cons "pattern" pat))
                        :timeout timeout
                      ) ;pi-execute-find
               ) ;files
              ) ;
          (if (or (string=? files "No files found matching pattern") (string-null? files))
            '()
            (map
              (lambda (f) (list (path->string (path-join (path abs-path) (path f))) f))
              (string-split files "\n")
            ) ;map
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (%format-grep-matches all-matches cache ctx-val limit limit-reached?)
      (let block-loop
        ((m-list all-matches) (out-lines '()) (any-line-trunc? #f))
        (if (null? m-list)
          (let* ((raw-out (string-join (reverse out-lines) "\n"))
                 (trunc (pi-truncate-head raw-out 999999 PI_DEFAULT_MAX_BYTES))
                ) ;
            (string-append (pi-trunc-result-content trunc)
              (%build-grep-notices limit-reached?
                limit
                (pi-trunc-result-truncated trunc)
                any-line-trunc?
              ) ;%build-grep-notices
            ) ;string-append
          ) ;let*
          (let* ((m (car m-list))
                 (rel-f (car m))
                 (line-n (cadr m))
                 (lines (cdr (assoc rel-f cache)))
                ) ;
            (let-values (((block trunc?) (%format-grep-block lines rel-f line-n ctx-val)))
              (block-loop (cdr m-list)
                (append (reverse block) out-lines)
                (or any-line-trunc? trunc?)
              ) ;block-loop
            ) ;let-values
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%run-grep-scan candidates literal? pattern ignore-case? ctx-val limit timeout)
      (let loop
        ((files candidates) (all-m '()) (cache '()) (count 0))
        (cond
         ((or (null? files) (>= count limit))
          (if (null? all-m)
            "No matches found"
            (%format-grep-matches all-m cache ctx-val limit (>= count limit))
          ) ;if
         ) ;
         (else
           (let* ((cand (car files))
                  (f-abs (car cand))
                  (f-rel (cadr cand))
                  (f-text (guard (e (else "")) (path-read-text (path f-abs))))
                  (f-lines (string-split f-text "\n"))
                  (new-cache (cons (cons f-rel f-lines) cache))
                  (rem-limit (- limit count))
                  (matches (if literal?
                             (%scan-file-literal f-lines f-rel pattern ignore-case? rem-limit)
                             (%scan-file-regex f-abs f-rel pattern ignore-case? timeout)
                           ) ;if
                  ) ;matches
                  (valid (if (> (length matches) rem-limit) (take matches rem-limit) matches))
                 ) ;
             (loop (cdr files) (append all-m valid) new-cache (+ count (length valid)))
           ) ;let*
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-execute-grep args . opts)
      (let* ((timed-out? (pi-keyword-value :timed-out? opts #f))
             (cwd (pi-keyword-value :cwd opts (getcwd)))
             (timeout (pi-keyword-value :timeout opts 120))
             (pattern (pi-arg-ref args "pattern"))
             (raw-path (pi-arg-ref args "path" "."))
             (glob-pat (pi-arg-ref args "glob" #f))
             (ignore-case? (pi-arg-ref args "ignoreCase" #f))
             (literal? (pi-arg-ref args "literal" #f))
             (effective-limit
               (let ((l (pi-arg-ref args "limit" 100)))
                 (if (number? l) l 100)
               ) ;let
             ) ;effective-limit
             (ctx-val
               (let ((c (pi-arg-ref args "context" 0)))
                 (if (and (number? c) (> c 0)) c 0)
               ) ;let
             ) ;ctx-val
            ) ;
        (when timed-out?
          (error 'error "Command timed out")
        ) ;when
        (unless (string? pattern)
          (error 'error "pattern must be a string")
        ) ;unless
        (let ((abs-path (pi-resolve-path raw-path cwd)))
          (unless (path-exists? (path abs-path))
            (error 'error (string-append "Path not found: " abs-path))
          ) ;unless
          (let* ((is-dir? (path-dir? (path abs-path)))
                 (candidates (%collect-grep-candidates abs-path is-dir? glob-pat timeout))
                ) ;
            (%run-grep-scan candidates literal? pattern ignore-case? ctx-val
              effective-limit timeout
            ) ;%run-grep-scan
          ) ;let*
        ) ;let
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
