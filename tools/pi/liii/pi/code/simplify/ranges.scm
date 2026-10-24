;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code simplify ranges)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii list)
    (liii path)
    (liii pi code read)
    (liii pi code bash)
    (liii pi code review files)
  ) ;import
  (export pi-parse-changed-ranges
    pi-diff-ranges-command
    pi-collect-simplify-files
  ) ;export
  (begin

    (define (%digit->number ch)
      (- (char->integer ch) (char->integer #\0))
    ) ;define

    (define (%parse-number text start end)
      (if (>= start end)
        #f
        (let loop
          ((i start) (acc 0))
          (if (>= i end)
            acc
            (let ((ch (string-ref text i)))
              (if (char-numeric? ch) (loop (+ i 1) (+ (* acc 10) (%digit->number ch))) #f)
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (%parse-hunk-header line)
      (let* ((trimmed (string-trim-both line))
             (plus (and (string-starts? trimmed "@@") (string-index trimmed #\+)))
            ) ;
        (if (not plus)
          #f
          (let* ((rest (substring trimmed (+ plus 1) (string-length trimmed)))
                 (comma (string-index rest #\,))
                 (space (string-index rest char-whitespace?))
                 (start-end (if comma comma (if space space (string-length rest))))
                 (start (%parse-number rest 0 start-end))
                 (count-end (if space space (string-length rest)))
                 (count (if comma (%parse-number rest (+ comma 1) count-end) 1))
                ) ;
            (if (or (not start) (not count) (zero? count))
              #f
              (list (cons 'start start) (cons 'end (+ start (- count 1))))
            ) ;if
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (%merge-range acc range)
      (if (null? acc)
        (list range)
        (let* ((prev (car acc))
               (prev-start (cdr (assoc 'start prev)))
               (prev-end (cdr (assoc 'end prev)))
               (start (cdr (assoc 'start range)))
               (end (cdr (assoc 'end range)))
              ) ;
          (if (<= start (+ prev-end 1))
            (cons (list (cons 'start prev-start) (cons 'end (max prev-end end))) (cdr acc))
            (cons range acc)
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (pi-parse-changed-ranges output)
      (reverse
        (fold
          (lambda (line acc)
            (let ((range (%parse-hunk-header line)))
              (if range (%merge-range acc range) acc)
            ) ;let
          ) ;lambda
          '()
          (string-split (or output "") "\n")
        ) ;fold
      ) ;reverse
    ) ;define

    (define (%safe-token? text)
      (string-every
        (lambda (ch)
          (or (char-alphabetic? ch)
            (char-numeric? ch)
            (memv ch '(#\. #\_ #\/ #\~ #\- #\^ #\@ #\+))
          ) ;or
        ) ;lambda
        text
      ) ;string-every
    ) ;define

    (define (%shell-quote text)
      (if (%safe-token? text)
        text
        (string-append "'" (string-replace text "'" "'\\''") "'")
      ) ;if
    ) ;define

    (define (pi-diff-ranges-command ref staged path)
      (string-append "git diff --unified=0 --no-ext-diff "
        (if (and staged (not (string=? ref "HEAD~1")))
          "--cached "
          (string-append (%shell-quote ref) " ")
        ) ;if
        "-- "
        (%shell-quote path)
      ) ;string-append
    ) ;define

    (define (%read-content file-path)
      (catch #t
        (lambda ()
          (if (path-exists? (path file-path)) (path-read-text (path file-path)) "")
        ) ;lambda
        (lambda _ "")
      ) ;catch
    ) ;define

    (define (%run-ranges runner command timeout)
      (catch #t
        (lambda ()
          (let-values (((text is-error?) (runner command timeout)))
            (cons text is-error?)
          ) ;let-values
        ) ;lambda
        (lambda _ (cons "" #t))
      ) ;catch
    ) ;define

    (define (%range-record entry ref staged? runner timeout)
      (let* ((file-path (cdr (assoc 'path entry)))
             (status (cdr (assoc 'status entry)))
             (added? (string=? status "added"))
            ) ;
        (if added?
          (list (cons 'path file-path)
            (cons 'status status)
            (cons 'ranges #f)
            (cons 'ranges-available? #t)
            (cons 'content (%read-content file-path))
          ) ;list
          (let* ((pair (%run-ranges runner (pi-diff-ranges-command ref staged? file-path) timeout)
                 ) ;pair
                 (ok? (not (cdr pair)))
                ) ;
            (list (cons 'path file-path)
              (cons 'status status)
              (cons 'ranges (and ok? (pi-parse-changed-ranges (car pair))))
              (cons 'ranges-available? ok?)
              (cons 'content (%read-content file-path))
            ) ;list
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-collect-simplify-files . opts)
      (let* ((ref (pi-keyword-value :ref opts "HEAD"))
             (staged (pi-keyword-value :staged opts #f))
             (files (pi-keyword-value :files opts #f))
             (runner (pi-keyword-value :runner opts #f))
             (timeout (pi-keyword-value :timeout opts 600))
             (max-files (pi-keyword-value :max-files opts PI_REVIEW_MAX_FILES))
             (collected (if runner
                          (pi-collect-changed-files
                            :ref       ref
                            :staged    staged
                            :files     files
                            :runner    runner
                            :timeout   timeout
                            :max-files max-files
                          ) ;pi-collect-changed-files
                          (pi-collect-changed-files
                            :ref       ref
                            :staged    staged
                            :files     files
                            :timeout   timeout
                            :max-files max-files
                          ) ;pi-collect-changed-files
                        ) ;if
             ) ;collected
             (entries (cdr (assoc 'entries collected)))
             (source (cdr (assoc 'source collected)))
             (range-ref (if (equal? source "fallback") "HEAD~1" ref))
             (range-staged? (and staged (not (equal? source "fallback"))))
             (run
               (or runner
                 (lambda (command timeout)
                   (pi-execute-bash (list (cons "command" command) (cons "timeout" timeout))
                     :timeout timeout
                   ) ;pi-execute-bash
                 ) ;lambda
               ) ;or
             ) ;run
            ) ;
        (append collected
          (list
            (cons 'files
              (map (lambda (entry) (%range-record entry range-ref range-staged? run timeout))
                entries
              ) ;map
            ) ;cons
          ) ;list
        ) ;append
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
