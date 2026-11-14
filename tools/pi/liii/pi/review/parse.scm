;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi review parse)
  (import (scheme base) (liii string) (liii list) (liii json))
  (export PI_REVIEW_SEVERITIES pi-parse-review-findings make-pi-review-finding
    pi-review-finding? pi-review-finding-severity pi-review-finding-file
    pi-review-finding-line pi-review-finding-category pi-review-finding-message
    pi-review-finding-suggestion
  ) ;export
  (begin

    (define PI_REVIEW_SEVERITIES '("CRITICAL" "HIGH" "MEDIUM" "INFO"))

    (define-record-type <pi-review-finding>
      (make-pi-review-finding severity file line category message suggestion)
      pi-review-finding?
      (severity pi-review-finding-severity)
      (file pi-review-finding-file)
      (line pi-review-finding-line)
      (category pi-review-finding-category)
      (message pi-review-finding-message)
      (suggestion pi-review-finding-suggestion)
    ) ;define-record-type

    (define (%jref obj key default)
      (if (not (pair? obj))
        default
        (let* ((str-key (if (symbol? key) (symbol->string key) key))
               (sym-key (if (string? key) (string->symbol key) key))
               (pair (or (assoc str-key obj) (assoc sym-key obj)))
              ) ;
          (if pair (cdr pair) default)
        ) ;let*
      ) ;if
    ) ;define

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
      (and (pair? item)
        (let ((severity (%jref item "severity" #f)))
          (and (string? severity)
            (member severity PI_REVIEW_SEVERITIES)
            (string? (%jref item "file" #f))
            (string? (%jref item "category" #f))
            (string? (%jref item "message" #f))
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    (define (%normalize-finding item)
      (let ((line (%jref item "line" #f)) (suggestion (%jref item "suggestion" #f)))
        (make-pi-review-finding (%jref item "severity" "")
          (%jref item "file" "")
          (if (number? line) line #f)
          (%jref item "category" "")
          (%jref item "message" "")
          (if (string? suggestion) suggestion #f)
        ) ;make-pi-review-finding
      ) ;let
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
          (let ((raw (if (vector? parsed) parsed (%jref parsed "findings" #f))))
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
