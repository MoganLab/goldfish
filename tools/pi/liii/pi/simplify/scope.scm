;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi simplify scope)
  (import (scheme base) (liii string) (liii list))
  (export pi-build-scope-lines pi-format-range)
  (begin

    (define (%field key entry default)
      (let ((pair (assoc key entry)))
        (if pair (cdr pair) default)
      ) ;let
    ) ;define

    (define (pi-format-range range)
      (let ((start (%field 'start range 0)) (end (%field 'end range 0)))
        (if (= start end)
          (number->string start)
          (string-append (number->string start) "-" (number->string end))
        ) ;if
      ) ;let
    ) ;define

    (define (%format-ranges ranges)
      (string-join (map pi-format-range ranges) ", ")
    ) ;define

    (define (pi-build-scope-line file)
      (let* ((path (%field 'path file ""))
             (status (%field 'status file "modified"))
             (ranges (%field 'ranges file #f))
             (available? (%field 'ranges-available? file #f))
            ) ;
        (cond ((string=? status "added")
               (string-append "- " path " (added; entire file is in scope)")
              ) ;
              ((not available?)
               (string-append "- " path " (" status
                 "; changed lines unavailable — inspect git diff before editing)"
               ) ;string-append
              ) ;
              ((null? ranges)
               (string-append "- " path " (" status
                 "; deletions only — no current lines to simplify)"
               ) ;string-append
              ) ;
              (else (string-append "- "
                      path
                      " ("
                      status
                      "; changed lines: "
                      (%format-ranges ranges)
                      ")"
                    ) ;string-append
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (pi-build-scope-lines files)
      (string-join (map pi-build-scope-line files) "\n")
    ) ;define

  ) ;begin
) ;define-library
