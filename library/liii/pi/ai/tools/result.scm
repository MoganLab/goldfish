;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai tools result)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii json)
    (liii list)
    (liii pi ai message block)
  ) ;import
  (export pi-tool-result pi-tool-result? pi-tool-result-content
    pi-tool-result-is-error pi-tool-result-isError pi-tool-result->json
    json->pi-tool-result
  ) ;export
  (begin

    (define-record-type <pi-tool-result>
      (%raw-pi-tool-result content is-error)
      pi-tool-result?
      (content pi-tool-result-content)
      (is-error pi-tool-result-is-error)
    ) ;define-record-type

    (define* (pi-tool-result content (is-error #f))
      (let ((eff-content
              (cond
               ((string? content) (vector (list (cons 'type "text") (cons 'text content))))
               ((vector? content) content)
               ((pair? content) (list->vector content))
               (else content)
              ) ;cond
            ) ;eff-content
           ) ;
        (%raw-pi-tool-result eff-content (if is-error #t #f))
      ) ;let
    ) ;define*

    (define pi-tool-result-isError pi-tool-result-is-error)

    (define (%json-object-entry? x)
      (and (pair? x) (not (pair? (car x))) (or (symbol? (car x)) (string? (car x))))
    ) ;define

    (define (%normalize-json-entry entry)
      (let* ((raw-k (car entry))
             (k (if (symbol? raw-k) (symbol->string raw-k) raw-k))
             (v (cdr entry))
            ) ;
        (cons k (%normalize-json-value v))
      ) ;let*
    ) ;define

    (define (%normalize-json-value x)
      (cond ((null? x) #())
            ((equal? x '(())) '(()))
            ((pi-block? x) (pi-block->json x))
            ((vector? x) (vector-map %normalize-json-value x))
            ((and (pair? x) (every %json-object-entry? x)) (map %normalize-json-entry x))
            ((pair? x) (list->vector (map %normalize-json-value x)))
            ((symbol? x) (symbol->string x))
            (else x)
      ) ;cond
    ) ;define

    (define (pi-tool-result->json res)
      (unless (pi-tool-result? res)
        (type-error "pi-tool-result->json: expected pi-tool-result" res)
      ) ;unless
      (list (cons "content" (%normalize-json-value (pi-tool-result-content res)))
        (cons "isError" (if (pi-tool-result-is-error res) 'true 'false))
      ) ;list
    ) ;define

    (define (%tool-convert-content c)
      (cond
       ((vector? c)
        (vector-map
          (lambda (x)
            (if (or (pi-block? x) (json-object? x)) (or (json->pi-block x) x) x)
          ) ;lambda
          c
        ) ;vector-map
       ) ;
       ((pair? c)
        (list->vector
          (map
            (lambda (x)
              (if (or (pi-block? x) (json-object? x)) (or (json->pi-block x) x) x)
            ) ;lambda
            c
          ) ;map
        ) ;list->vector
       ) ;
       (else #())
      ) ;cond
    ) ;define

    (define (json->pi-tool-result obj)
      (if (pi-tool-result? obj)
        obj
        (begin
          (unless (or (json-object? obj) (null? obj))
            (type-error "json->pi-tool-result: expected json object or pi-tool-result" obj)
          ) ;unless
          (if (null? obj)
            (pi-tool-result #() #f)
            (pi-tool-result (%tool-convert-content (json-ref obj "content"))
              (json-ref-boolean obj "isError" #f)
            ) ;pi-tool-result
          ) ;if
        ) ;begin
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
