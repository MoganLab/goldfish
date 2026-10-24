;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code review format)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii sort)
    (liii pi code review parse)
  ) ;import
  (export pi-format-review-findings)
  (begin

    (define (%field key entry default)
      (let ((pair (assoc key entry)))
        (if pair (cdr pair) default)
      ) ;let
    ) ;define

    (define (%severity-order severity)
      (or (list-index (lambda (s) (string=? s severity)) PI_REVIEW_SEVERITIES) 99)
    ) ;define

    (define (%sorted findings)
      (list-sort
        (lambda (a b)
          (< (%severity-order (%field 'severity a ""))
            (%severity-order (%field 'severity b ""))
          ) ;<
        ) ;lambda
        findings
      ) ;list-sort
    ) ;define

    (define (%format-location finding)
      (let ((file (%field 'file finding "")) (line (%field 'line finding #f)))
        (if line (string-append file ":" (number->string line)) file)
      ) ;let
    ) ;define

    (define (%format-finding finding)
      (let* ((suggestion (%field 'suggestion finding #f))
             (line (string-append "- **"
                     (%field 'severity finding "")
                     "** ["
                     (%field 'category finding "")
                     "] "
                     (%format-location finding)
                     ": "
                     (%field 'message finding "")
                   ) ;string-append
             ) ;line
            ) ;
        (if suggestion (string-append line "\n  Suggestion: " suggestion) line)
      ) ;let*
    ) ;define

    (define (%count-severity findings severity)
      (count (lambda (f) (string=? (%field 'severity f "") severity)) findings)
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
