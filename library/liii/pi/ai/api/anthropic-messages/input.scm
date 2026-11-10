;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api anthropic-messages input)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii json)
    (liii pi ai message)
    (liii pi ai event)
  ) ;import
  (export anthropic-normalize-input
    anthropic-parse-tool-arguments
    anthropic-assistant-blk->anthropic-blk
  ) ;export
  (begin

    (define (%val key obj default-val)
      (let ((p
              (or (assoc key obj)
                (assoc (if (symbol? key) (symbol->string key) (string->symbol key)) obj)
              ) ;or
            ) ;p
           ) ;
        (if (pair? p) (cdr p) default-val)
      ) ;let
    ) ;define

    (define (%object-entry? x)
      (and (pair? x) (or (symbol? (car x)) (string? (car x))))
    ) ;define

    (define (%entry-key raw-k)
      (if (symbol? raw-k) (symbol->string raw-k) raw-k)
    ) ;define

    (define (%alist-like? v)
      (and (list? v) (pair? (car v)) (or (symbol? (caar v)) (string? (caar v))))
    ) ;define

    (define (%normalize-entry-val v)
      (cond ((null? v) '(()))
            ((pair? v)
             (if (or (equal? v '(())) (%alist-like? v)) (anthropic-normalize-input v) v)
            ) ;
            (else v)
      ) ;cond
    ) ;define

    (define (%normalize-one-entry entry)
      (if (%object-entry? entry)
        (cons (%entry-key (car entry)) (%normalize-entry-val (cdr entry)))
        #f
      ) ;if
    ) ;define

    (define (%normalize-entries entries)
      (let ((acc (filter (lambda (e) e) (map %normalize-one-entry entries))))
        (if (null? acc) '(()) acc)
      ) ;let
    ) ;define

    (define (%normalize-string-input trimmed)
      (if (string-null? trimmed)
        '(())
        (catch #t
          (lambda () (anthropic-normalize-input (string->json trimmed)))
          (lambda _ '(()))
        ) ;catch
      ) ;if
    ) ;define

    (define (anthropic-normalize-input raw)
      (cond
       ((or (null? raw) (equal? raw '(())) (not raw) (eq? raw 'null)) '(()))
       ((string? raw) (%normalize-string-input (string-trim-both raw)))
       ((and (pair? raw) (list? raw)) (%normalize-entries raw))
       (else '(()))
      ) ;cond
    ) ;define

    (define (%parse-entry-val v)
      (cond
       ((or (null? v) (equal? v '(()))) '())
       ((and (pair? v) (%alist-like? v)) (anthropic-parse-tool-arguments v))
       (else v)
      ) ;cond
    ) ;define

    (define (%parse-one-entry entry)
      (if (%object-entry? entry)
        (cons (%entry-key (car entry)) (%parse-entry-val (cdr entry)))
        #f
      ) ;if
    ) ;define

    (define (%parse-entries entries)
      (filter (lambda (e) e) (map %parse-one-entry entries))
    ) ;define

    (define (%parse-string-arguments trimmed)
      (if (string-null? trimmed)
        '()
        (catch #t
          (lambda () (anthropic-parse-tool-arguments (string->json trimmed)))
          (lambda _ '())
        ) ;catch
      ) ;if
    ) ;define

    (define (anthropic-parse-tool-arguments raw)
      (cond
       ((or (null? raw) (equal? raw '(())) (not raw) (eq? raw 'null)) '())
       ((string? raw) (%parse-string-arguments (string-trim-both raw)))
       ((and (pair? raw) (list? raw)) (%parse-entries raw))
       (else '())
      ) ;cond
    ) ;define

    (define (anthropic-assistant-blk->anthropic-blk blk)
      (let ((btype (%val 'type blk "")))
        (cond
         ((equal? btype "text")
          (list (cons "type" "text") (cons "text" (%val 'text blk "")))
         ) ;
         ((equal? btype "toolCall")
          (list (cons "type" "tool_use")
            (cons "id" (%val 'id blk ""))
            (cons "name" (%val 'name blk ""))
            (cons "input" (anthropic-normalize-input (%val 'arguments blk '())))
          ) ;list
         ) ;
         ((equal? btype "thinking")
          (let ((sig (%val 'thinkingSignature blk #f)) (th (%val 'thinking blk "")))
            (if (and (string? sig) (not (string-null? sig)))
              (list (cons "type" "thinking") (cons "thinking" th) (cons "signature" sig))
              (list (cons "type" "thinking") (cons "thinking" th))
            ) ;if
          ) ;let
         ) ;
         (else (list (cons "type" "text") (cons "text" "")))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
