;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi simplify format)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii sort)
    (liii pi simplify parse)
  ) ;import
  (export pi-format-simplify-suggestions)
  (begin

    (define (%before? a b)
      (let ((file-a (pi-simplify-suggestion-file a))
            (file-b (pi-simplify-suggestion-file b))
            (line-a (pi-simplify-suggestion-line a))
            (line-b (pi-simplify-suggestion-line b))
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
      (let ((file (pi-simplify-suggestion-file entry))
            (line (pi-simplify-suggestion-line entry))
           ) ;
        (if line (string-append file ":" (number->string line)) file)
      ) ;let
    ) ;define

    (define (%format-entry entry)
      (let* ((suggestion (pi-simplify-suggestion-suggestion entry))
             (line (string-append "- ["
                     (pi-simplify-suggestion-category entry)
                     "] "
                     (%location entry)
                     ": "
                     (pi-simplify-suggestion-message entry)
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
