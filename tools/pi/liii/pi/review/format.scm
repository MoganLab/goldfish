;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi review format)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii sort)
    (liii pi review parse)
  ) ;import
  (export pi-format-review-findings)
  (begin

    (define (%severity-order severity)
      (or (list-index (lambda (s) (string=? s severity)) PI_REVIEW_SEVERITIES) 99)
    ) ;define

    (define (%sorted findings)
      (list-sort
        (lambda (a b)
          (< (%severity-order (pi-review-finding-severity a))
            (%severity-order (pi-review-finding-severity b))
          ) ;<
        ) ;lambda
        findings
      ) ;list-sort
    ) ;define

    (define (%format-location finding)
      (let ((file (pi-review-finding-file finding))
            (line (pi-review-finding-line finding))
           ) ;
        (if line (string-append file ":" (number->string line)) file)
      ) ;let
    ) ;define

    (define (%format-finding finding)
      (let* ((suggestion (pi-review-finding-suggestion finding))
             (line (string-append "- **"
                     (pi-review-finding-severity finding)
                     "** ["
                     (pi-review-finding-category finding)
                     "] "
                     (%format-location finding)
                     ": "
                     (pi-review-finding-message finding)
                   ) ;string-append
             ) ;line
            ) ;
        (if suggestion (string-append line "\n  Suggestion: " suggestion) line)
      ) ;let*
    ) ;define

    (define (%count-severity findings severity)
      (count (lambda (f) (string=? (pi-review-finding-severity f) severity)) findings)
    ) ;define

    (define (%summary findings)
      (string-join
        (filter (lambda (s) (not (string-null? s)))
          (map
            (lambda (severity)
              (let ((n (%count-severity findings severity)))
                (if (= n 0) "" (string-append (number->string n) " " severity))
              ) ;let
            ) ;lambda
            PI_REVIEW_SEVERITIES
          ) ;map
        ) ;filter
        ", "
      ) ;string-join
    ) ;define

    (define (pi-format-review-findings findings)
      (if (null? findings)
        "Code review complete: no issues found. Code looks clean."
        (string-append "## Code Review Findings ("
          (%summary findings)
          ")\n\n"
          (string-join (map %format-finding (%sorted findings)) "\n")
        ) ;string-append
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
