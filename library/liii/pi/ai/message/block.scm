;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai message block)
  (import (scheme base)
    (liii error)
    (liii json)
    (liii list)
    (liii pi ai message text-block)
    (liii pi ai message thinking-block)
    (liii pi ai message tool-call-block)
    (liii pi ai message image-block)
  ) ;import
  (export pi-block? pi-block-type pi-block->json json->pi-block
    %convert-message-content %normalize-json-value
  ) ;export
  (begin

    (define (pi-block? x)
      (or (pi-text-block? x)
        (pi-thinking-block? x)
        (pi-tool-call-block? x)
        (pi-image-block? x)
      ) ;or
    ) ;define

    (define (pi-block-type blk)
      (cond ((pi-text-block? blk) (pi-text-block-type blk))
            ((pi-thinking-block? blk) (pi-thinking-block-type blk))
            ((pi-tool-call-block? blk) (pi-tool-call-block-type blk))
            ((pi-image-block? blk) (pi-image-block-type blk))
            ((pair? blk)
             (let ((p (or (assoc "type" blk) (assoc 'type blk))))
               (if p (cdr p) #f)
             ) ;let
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (%json-object-entry? x)
      (and (pair? x) (not (pair? (car x))) (or (symbol? (car x)) (string? (car x))))
    ) ;define

    (define (%normalize-json-entry entry)
      (let* ((raw-k (car entry))
             (k (if (symbol? raw-k) (symbol->string raw-k) raw-k))
             (v (cdr entry))
            ) ;
        (cond ((and (string=? k "errorMessage") (not v)) #f)
              ((and (or (string=? k "arguments") (string=? k "input"))
                 (or (null? v) (equal? v '(())) (not v) (eq? v 'null))
               ) ;and
               (cons k '(()))
              ) ;
              (else (cons k (%normalize-json-value v)))
        ) ;cond
      ) ;let*
    ) ;define

    (define (%normalize-tool-args args)
      (if (or (not args) (null? args) (eq? args 'null) (equal? args '(())))
        '(())
        args
      ) ;if
    ) ;define

    (define (%normalize-json-value x)
      (cond ((null? x) #())
            ((equal? x '(())) '(()))
            ((pi-block? x) (pi-block->json x))
            ((vector? x) (vector-map %normalize-json-value x))
            ((and (pair? x) (every %json-object-entry? x))
             (filter (lambda (entry) entry) (map %normalize-json-entry x))
            ) ;
            ((pair? x) (list->vector (map %normalize-json-value x)))
            ((symbol? x) (symbol->string x))
            (else x)
      ) ;cond
    ) ;define

    (define (pi-block->json blk)
      (unless (pi-block? blk)
        (type-error "pi-block->json: expected pi-block" blk)
      ) ;unless
      (cond
       ((pi-text-block? blk)
        (let ((base (list (cons "type" "text") (cons "text" (pi-text-block-text blk))))
              (sig (pi-text-block-signature blk))
             ) ;
          (if sig (append base (list (cons "textSignature" sig))) base)
        ) ;let
       ) ;
       ((pi-thinking-block? blk)
        (let ((base
                (list (cons "type" "thinking")
                  (cons "thinking" (pi-thinking-block-thinking blk))
                ) ;list
              ) ;base
              (sig (pi-thinking-block-thinking-signature blk))
             ) ;
          (if sig (append base (list (cons "thinkingSignature" sig))) base)
        ) ;let
       ) ;
       ((pi-tool-call-block? blk)
        (let ((base
                (list (cons "type" "toolCall")
                  (cons "id" (pi-tool-call-block-id blk))
                  (cons "name" (pi-tool-call-block-name blk))
                  (cons "arguments"
                    (%normalize-json-value (%normalize-tool-args (pi-tool-call-block-arguments blk)))
                  ) ;cons
                ) ;list
              ) ;base
              (sig (pi-tool-call-block-thought-signature blk))
             ) ;
          (if sig (append base (list (cons "thoughtSignature" sig))) base)
        ) ;let
       ) ;
       ((pi-image-block? blk)
        (list (cons "type" "image")
          (cons "data" (pi-image-block-data blk))
          (cons "mimeType" (pi-image-block-mime-type blk))
        ) ;list
       ) ;
      ) ;cond
    ) ;define

    (define (%string-key-json-object? obj)
      (and (json-object? obj)
        (or (equal? obj '(())) (every (lambda (p) (and (pair? p) (string? (car p)))) obj))
      ) ;and
    ) ;define

    (define (json->pi-block obj)
      (if (pi-block? obj)
        obj
        (begin
          (unless (or (%string-key-json-object? obj) (null? obj))
            (type-error "json->pi-block: expected json object or pi-block" obj)
          ) ;unless
          (if (null? obj)
            #f
            (let ((btype (json-ref obj "type")))
              (cond
               ((equal? btype "text")
                (let ((raw-sig (json-ref obj "textSignature")))
                  (pi-text-block (json-ref-string obj "text" "")
                    (if (string? raw-sig) raw-sig #f)
                  ) ;pi-text-block
                ) ;let
               ) ;
               ((equal? btype "thinking")
                (let ((raw-sig (json-ref obj "thinkingSignature")))
                  (pi-thinking-block (json-ref-string obj "thinking" "")
                    (if (string? raw-sig) raw-sig #f)
                  ) ;pi-thinking-block
                ) ;let
               ) ;
               ((equal? btype "toolCall")
                (let ((raw-args (json-ref obj "arguments"))
                      (raw-sig (json-ref obj "thoughtSignature"))
                     ) ;
                  (pi-tool-call-block (json-ref-string obj "id" "")
                    (json-ref-string obj "name" "")
                    (%normalize-tool-args (if (null? raw-args) '() raw-args))
                    (if (string? raw-sig) raw-sig #f)
                  ) ;pi-tool-call-block
                ) ;let
               ) ;
               ((equal? btype "image")
                (let ((m (json-ref-string obj "media_type" "image/png"))
                      (m2 (json-ref obj "mimeType"))
                     ) ;
                  (pi-image-block (json-ref-string obj "data" "")
                    (if (string? m2) (if (string=? m2 "") m m2) m)
                  ) ;pi-image-block
                ) ;let
               ) ;
               (else #f)
              ) ;cond
            ) ;let
          ) ;if
        ) ;begin
      ) ;if
    ) ;define

    (define (%convert-block-elem x)
      (if (or (pi-block? x) (%string-key-json-object? x)) (or (json->pi-block x) x) x)
    ) ;define

    (define (%convert-message-content c)
      (cond ((vector? c) (vector-map %convert-block-elem c))
            ((and (pair? c) (not (pair? (car c)))) c)
            ((pair? c) (map %convert-block-elem c))
            (else c)
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
