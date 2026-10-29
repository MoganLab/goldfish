;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core tools grep)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii list)
    (liii os)
    (liii path)
    (liii subprocess)
    (liii either)
    (liii pi code core tools bash)
    (liii pi code core tools read)
    (liii pi code core tools find)
    (liii pi code core failure)
    (liii pi code core tools grep format)
  ) ;import
  (export pi-grep-schema pi-execute-grep %read-grep-lines)
  (begin

    (define (%prop name type desc)
      (cons name (list (cons "type" type) (cons "description" desc)))
    ) ;define

    (define (pi-grep-schema)
      (list (cons "type" "object")
        (cons "properties"
          (list (%prop "pattern" "string" "Search pattern (regex or literal string)")
            (%prop "path"
              "string"
              "Directory or file to search (default: current directory)"
            ) ;%prop
            (%prop "glob"
              "string"
              "Filter files by glob pattern, e.g. '*.ts' or '**/*.spec.ts'"
            ) ;%prop
            (%prop "ignoreCase" "boolean" "Case-insensitive search (default: false)")
            (%prop "literal"
              "boolean"
              "Treat pattern as literal string instead of regex (default: false)"
            ) ;%prop
            (%prop "context"
              "number"
              "Number of lines to show before and after each match (default: 0)"
            ) ;%prop
            (%prop "limit" "number" "Maximum number of matches to return (default: 100)")
          ) ;list
        ) ;cons
        (cons "required" #("pattern"))
      ) ;list
    ) ;define

    ;; 读取候选文件并按行切分；读不出来（文件被删除、无权限、是目录等）时
    ;; 返回 #f，交由调用方记为「已跳过」——静默当成空文件会让「无匹配」
    ;; 掩盖真实失败（Issue #1189）
    (define (%read-grep-lines p)
      (let ((lines (guard (e (else #f)) (path-read-lines p))))
        (if (list? lines) lines #f)
      ) ;let
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
         ((and (>= (length parts) 3) (string->number (cadr parts)))
          (list rel-path (string->number (cadr parts)))
         ) ;
         ((and (>= (length parts) 4) (string->number (caddr parts)))
          (list rel-path (string->number (caddr parts)))
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
             (res (run-either args :stdout 'capture :stderr 'capture :timeout timeout))
            ) ;
        (if (either-right? res)
          (let ((lines (string-split (to-right res) "\n")))
            (filter (lambda (p) p) (map (lambda (l) (%parse-grep-line l rel-path)) lines))
          ) ;let
          (let ((e (to-left res)))
            ;; grep 退出码 1 表示无匹配，其余非零退出码为真实失败
            (if (= (process-error-code e) 1) '() (pi-command-error "grep" e))
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (%collect-grep-candidates abs-path is-dir? glob-pat timeout)
      (if (not is-dir?)
        (if (path-file? (path abs-path))
          (list (list abs-path (path-name (path abs-path))))
          '()
        ) ;if
        (let ((files
                (pi-execute-find (list (cons "path" abs-path) (cons "pattern" (or glob-pat "*")))
                  :timeout timeout
                ) ;pi-execute-find
              ) ;files
             ) ;
          (if (or (string=? files "No files found matching pattern") (string-null? files))
            '()
            (filter
              (lambda (c) (path-file? (path (car c))))
              (map
                (lambda (f) (list (path->string (path-join (path abs-path) (path f))) f))
                (string-split files "\n")
              ) ;map
            ) ;filter
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; 扫描单个候选文件：返回 (行列表 匹配列表)
    ;; 文件已消失或读不出来时行列表为 #f，调用方据此记为「已跳过」——
    ;; 静默当成空文件会让「无匹配」掩盖真实失败（Issue #1189）
    (define (%scan-grep-file f-abs f-rel pattern literal? ignore-case? rem-limit timeout)
      (if (not (path-file? (path f-abs)))
        (values #f '())
        (let ((f-lines (%read-grep-lines (path f-abs))))
          (if (not f-lines)
            (values #f '())
            (values f-lines
              (if literal?
                (%scan-file-literal f-lines f-rel pattern ignore-case? rem-limit)
                (%scan-file-regex f-abs f-rel pattern ignore-case? timeout)
              ) ;if
            ) ;values
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (%run-grep-scan candidates literal? pattern ignore-case? ctx-val limit timeout)
      (let loop
        ((files candidates) (all-m '()) (cache '()) (count 0) (skipped '()))
        (cond
         ((or (null? files) (>= count limit))
          (if (null? all-m)
            (%grep-empty-result skipped)
            (%format-grep-matches all-m cache ctx-val limit (>= count limit) skipped)
          ) ;if
         ) ;
         (else
           (let* ((cand (car files))
                  (f-abs (car cand))
                  (f-rel (cadr cand))
                  (rem-limit (- limit count))
                 ) ;
             (let-values (((f-lines matches)
                           (%scan-grep-file f-abs f-rel pattern literal? ignore-case? rem-limit timeout)
                          ) ;
                         ) ;
               (if (not f-lines)
                 (loop (cdr files) all-m cache count (cons f-rel skipped))
                 (let* ((new-cache (cons (cons f-rel f-lines) cache))
                        (valid (if (> (length matches) rem-limit) (take matches rem-limit) matches))
                       ) ;
                   (loop (cdr files)
                     (append all-m valid)
                     new-cache
                     (+ count (length valid))
                     skipped
                   ) ;loop
                 ) ;let*
               ) ;if
             ) ;let-values
           ) ;let*
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-execute-grep args . opts)
      (let* ((timed-out? (pi-keyword-value :timed-out? opts #f))
             (cwd (pi-keyword-value :cwd opts (getcwd)))
             (timeout (pi-keyword-value :timeout opts 600))
             (pattern (pi-arg-ref args "pattern"))
             (raw-path (pi-arg-ref args "path" "."))
             (glob-pat (pi-arg-ref args "glob" #f))
             (ignore-case? (pi-arg-ref args "ignoreCase" #f))
             (literal? (pi-arg-ref args "literal" #f))
             (l (pi-int-arg (pi-arg-ref args "limit" 100)))
             (effective-limit (if l l 100))
             (c (pi-int-arg (pi-arg-ref args "context" 0)))
             (ctx-val (if (and c (> c 0)) c 0))
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
