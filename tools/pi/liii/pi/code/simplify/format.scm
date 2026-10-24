;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code simplify format)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii sort)
    (liii pi code simplify parse)
  ) ;import
  (export pi-format-simplify-suggestions)
  (begin

    (define (%field key entry default)
      (let ((pair (assoc key entry)))
        (if pair (cdr pair) default)
      ) ;let
    ) ;define

    (define (%line-of entry)
      (%field 'line entry #f)
    ) ;define

    (define (%before? a b)
      (let ((file-a (%field 'file a ""))
            (file-b (%field 'file b ""))
            (line-a (%line-of a))
            (line-b (%line-of b))
           ) ;
        (cond ((string<? file-a file-b) #t)
              ((string>? file-a file-b) #f)
              ((not line-a) #f)
              ((not line-b) #t)
              (else (< line-a line-b))
        ) ;cond
      ) ;let
    ) ;define

    (define (%sorted suggestions)
      (list-sort %before? suggestions)
    ) ;define

    (define (%location entry)
      (let ((file (%field 'file entry "")) (line (%line-of entry)))
        (if line (string-append file ":" (number->string line)) file)
      ) ;let
    ) ;define

    (define (%format-entry entry)
      (let* ((suggestion (%field 'suggestion entry #f))
             (line (string-append "- ["
                     (%field 'category entry "")
                     "] "
                     (%location entry)
                     ": "
                     (%field 'message entry "")
                   ) ;string-append
             ) ;line
            ) ;
        (if (and (string? suggestion) (not (string-null? suggestion)))
          (string-append line "\n  Suggestion: " suggestion)
          line
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-format-simplify-suggestions suggestions)
      (if (null? suggestions)
        "No simplification opportunities found. Code looks clean."
        (string-append "## Simplify Suggestions ("
          (number->string (length suggestions))
          ")\n\n"
          (string-join (map %format-entry (%sorted suggestions)) "\n")
        ) ;string-append
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
