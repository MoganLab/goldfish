;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi review parse)
  (import (scheme base) (liii base) (liii string) (liii list) (liii json))
  (export PI_REVIEW_SEVERITIES pi-parse-review-findings make-pi-review-finding
    pi-review-finding? pi-review-finding-severity pi-review-finding-file
    pi-review-finding-line pi-review-finding-category pi-review-finding-message
    pi-review-finding-suggestion
  ) ;export
  (begin

    (define PI_REVIEW_SEVERITIES '("CRITICAL" "HIGH" "MEDIUM" "INFO"))

    (define*-record-type <pi-review-finding>
      (make-pi-review-finding severity
        file
        (line #f)
        (category "")
        (message "")
        (suggestion #f)
      ) ;make-pi-review-finding
      pi-review-finding?
      (severity pi-review-finding-severity)
      (file pi-review-finding-file)
      (line pi-review-finding-line)
      (category pi-review-finding-category)
      (message pi-review-finding-message)
      (suggestion pi-review-finding-suggestion)
    ) ;define*-record-type

    (define (%strip-fence text)
      (let ((trimmed (string-trim-both text)))
        (if (string-starts? trimmed "```")
          (let* ((first-newline (string-index trimmed #\newline))
                 (body
                   (if first-newline
                     (string-trim-both (substring trimmed (+ first-newline 1) (string-length trimmed))
                     ) ;string-trim-both
                     ""
                   ) ;if
                 ) ;body
                ) ;
            (if (string-ends? body "```") (string-remove-suffix body "```") body)
          ) ;let*
          trimmed
        ) ;if
      ) ;let
    ) ;define

    (define (%extract-object text)
      (let* ((trimmed (string-trim-both text))
             (start (string-index trimmed #\{))
             (end (string-index-right trimmed #\}))
            ) ;
        (if (and start end (> end start)) (substring trimmed start (+ end 1)) trimmed)
      ) ;let*
    ) ;define

    (define (%truncate-raw text)
      (if (<= (string-length text) 200) text (string-take text 200))
    ) ;define

    (define (%fallback text)
      (list (make-pi-review-finding "INFO"
              ""
              #f
              "parse-error"
              (string-append "Could not parse structured review. Raw response: "
                (%truncate-raw text)
              ) ;string-append
              #f
            ) ;make-pi-review-finding
      ) ;list
    ) ;define

    (define (%vector-or-list->list value)
      (cond ((vector? value) (vector->list value))
            ((list? value) value)
            (else '())
      ) ;cond
    ) ;define

    (define (%valid-finding? item)
      (and (json-object? item)
        (let ((severity (json-ref-string item "severity" #f)))
          (and severity
            (member severity PI_REVIEW_SEVERITIES)
            (string? (json-ref-string item "file" #f))
            (string? (json-ref-string item "category" #f))
            (string? (json-ref-string item "message" #f))
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    (define (%normalize-finding item)
      (make-pi-review-finding (json-ref-string item "severity" "")
        (json-ref-string item "file" "")
        (json-ref-number item "line" #f)
        (json-ref-string item "category" "")
        (json-ref-string item "message" "")
        (json-ref-string item "suggestion" #f)
      ) ;make-pi-review-finding
    ) ;define

    (define (%extract-findings text)
      (let ((parsed
              (catch #t
                (lambda () (string->json (%extract-object (%strip-fence text))))
                (lambda _ #f)
              ) ;catch
            ) ;parsed
           ) ;
        (if (not (pair? parsed))
          #f
          (let ((raw (if (vector? parsed)
                       parsed
                       (and (json-contains-key? parsed "findings") (json-ref parsed "findings"))
                     ) ;if
                ) ;raw
               ) ;
            (if raw
              (filter (lambda (f) f)
                (map (lambda (item) (and (%valid-finding? item) (%normalize-finding item)))
                  (%vector-or-list->list raw)
                ) ;map
              ) ;filter
              #f
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (pi-parse-review-findings text)
      (let ((findings (%extract-findings (or text ""))))
        (if findings findings (%fallback (or text "")))
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
