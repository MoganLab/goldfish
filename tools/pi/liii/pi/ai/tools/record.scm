;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai tools record)
  (import (scheme base) (liii pi ai tools types))
  (export pi-tool-definition pi-tool-definition? pi-tool-definition-name
    pi-tool-definition-description pi-tool-definition-parameters
    pi-tool-definition-execute pi-tool-definition-snippet
    pi-tool-definition-guidelines pi-tool-definition->alist pi-tool-result
    pi-tool-result? pi-tool-result-content pi-tool-result-is-error
    pi-tool-result-isError pi-tool-result->alist pi-trunc-result
    pi-trunc-result? pi-trunc-result-content pi-trunc-result-truncated
    pi-trunc-result-truncated? pi-trunc-result-truncated-by
    pi-trunc-result-total-lines pi-trunc-result-total-bytes
    pi-trunc-result-output-lines pi-trunc-result-output-bytes
    pi-trunc-result-first-line-exceeds-limit?
    pi-trunc-result-first-line-truncated pi-trunc-result-last-line-partial?
    pi-trunc-result-last-line-truncated pi-trunc-result->alist
  ) ;export
  (begin

    (define* (pi-tool-definition name
               description
               parameters
               execute
               (snippet "")
               (guidelines '())
             ) ;pi-tool-definition
      (%make-pi-tool-definition name description parameters execute snippet
        guidelines
      ) ;%make-pi-tool-definition
    ) ;define*

    (define (pi-tool-definition->alist def)
      (if (pair? def)
        def
        (list (cons 'name (pi-tool-definition-name def))
          (cons 'description (pi-tool-definition-description def))
          (cons 'parameters (pi-tool-definition-parameters def))
          (cons 'execute (pi-tool-definition-execute def))
          (cons 'snippet (pi-tool-definition-snippet def))
          (cons 'guidelines (pi-tool-definition-guidelines def))
        ) ;list
      ) ;if
    ) ;define

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
        (%make-pi-tool-result eff-content (if is-error #t #f))
      ) ;let
    ) ;define*

    (define pi-tool-result-isError pi-tool-result-is-error)

    (define (pi-tool-result->alist res)
      (if (pair? res)
        res
        (list (cons 'content (pi-tool-result-content res))
          (cons 'isError (pi-tool-result-is-error res))
        ) ;list
      ) ;if
    ) ;define

    (define* (pi-trunc-result content
               truncated
               (truncated-by #f)
               (total-lines 0)
               (total-bytes 0)
               (output-lines 0)
               (output-bytes 0)
               (first-line-exceeds-limit? #f)
               (last-line-partial? #f)
             ) ;pi-trunc-result
      (%make-pi-trunc-result content
        (if truncated #t #f)
        truncated-by
        total-lines
        total-bytes
        output-lines
        output-bytes
        (if first-line-exceeds-limit? #t #f)
        (if last-line-partial? #t #f)
      ) ;%make-pi-trunc-result
    ) ;define*

    (define pi-trunc-result-truncated? pi-trunc-result-truncated)

    (define pi-trunc-result-first-line-truncated
      pi-trunc-result-first-line-exceeds-limit?
    ) ;define

    (define pi-trunc-result-last-line-truncated pi-trunc-result-last-line-partial?)

    (define (pi-trunc-result->alist r)
      (if (pair? r)
        r
        (list (cons 'content (pi-trunc-result-content r))
          (cons 'truncated (pi-trunc-result-truncated r))
          (cons 'truncated-by (pi-trunc-result-truncated-by r))
          (cons 'total-lines (pi-trunc-result-total-lines r))
          (cons 'total-bytes (pi-trunc-result-total-bytes r))
          (cons 'output-lines (pi-trunc-result-output-lines r))
          (cons 'output-bytes (pi-trunc-result-output-bytes r))
          (cons 'first-line-exceeds-limit? (pi-trunc-result-first-line-exceeds-limit? r))
          (cons 'first-line-truncated (pi-trunc-result-first-line-exceeds-limit? r))
          (cons 'last-line-partial? (pi-trunc-result-last-line-partial? r))
          (cons 'last-line-truncated (pi-trunc-result-last-line-partial? r))
        ) ;list
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
