;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core compaction serialize)
  (import (scheme base)
    (scheme write)
    (liii string)
    (liii list)
    (liii pi ai message)
  ) ;import
  (export pi-truncate-for-summary pi-serialize-conversation pi-content->text)
  (begin

    (define (%ref obj key)
      (let* ((str-key (if (symbol? key) (symbol->string key) key))
             (sym-key (if (string? key) (string->symbol key) key))
             (pair (or (assoc str-key obj) (assoc sym-key obj)))
            ) ;
        (if pair (cdr pair) #f)
      ) ;let*
    ) ;define

    (define (%normalize-message msg)
      (if (pi-message? msg) (pi-message->alist msg) msg)
    ) ;define

    (define (%blocks content)
      (cond ((vector? content) (vector->list content))
            ((null? content) '())
            ((pair? content) content)
            (else '())
      ) ;cond
    ) ;define

    (define (%block-text blk)
      (if (pi-block? blk) (pi-block->alist blk) blk)
    ) ;define

    (define (pi-content->text content)
      (if (string? content)
        content
        (string-join
          (filter
            (lambda (t) (and (string? t) (not (string-null? t))))
            (map
              (lambda (blk) (and (equal? (%ref blk "type") "text") (%ref blk "text")))
              (map %block-text (%blocks content))
            ) ;map
          ) ;filter
          ""
        ) ;string-join
      ) ;if
    ) ;define

    (define (%json-value v)
      (cond ((string? v) (string-append "\"" v "\""))
            ((number? v) (number->string v))
            ((eq? v #t) "true")
            ((eq? v #f) "false")
            (else (object->string v))
      ) ;cond
    ) ;define

    (define (%format-args args)
      (if (not (pair? args))
        ""
        (let ((pairs
                (if
                  (and (pair? (car args)) (or (string? (caar args)) (symbol? (caar args))))
                  args
                  '()
                ) ;if
              ) ;pairs
             ) ;
          (string-join
            (map
              (lambda (p)
                (string-append (if (symbol? (car p)) (symbol->string (car p)) (car p))
                  "="
                  (%json-value (cdr p))
                ) ;string-append
              ) ;lambda
              pairs
            ) ;map
            ", "
          ) ;string-join
        ) ;let
      ) ;if
    ) ;define

    (define (%assistant-tool-calls content)
      (map
        (lambda (blk)
          (string-append (%ref blk "name") "(" (%format-args (%ref blk "arguments")) ")")
        ) ;lambda
        (filter (lambda (blk) (equal? (%ref blk "type") "toolCall"))
          (map %block-text (%blocks content))
        ) ;filter
      ) ;map
    ) ;define

    (define (%has-text-block? content)
      (any
        (lambda (blk)
          (and (equal? (%ref blk "type") "text") (string? (%ref blk "text")))
        ) ;lambda
        (map %block-text (%blocks content))
      ) ;any
    ) ;define

    (define (pi-truncate-for-summary text max-chars)
      (if (<= (string-length text) max-chars)
        text
        (string-append (substring text 0 max-chars)
          "\n\n[... "
          (number->string (- (string-length text) max-chars))
          " more characters truncated]"
        ) ;string-append
      ) ;if
    ) ;define

    (define (%assistant-parts content text)
      (let ((calls (%assistant-tool-calls content)))
        (append
          (if (and (%has-text-block? content) (not (string-null? text)))
            (list (string-append "[Assistant]: " text))
            '()
          ) ;if
          (if (pair? calls)
            (list (string-append "[Assistant tool calls]: " (string-join calls "; ")))
            '()
          ) ;if
        ) ;append
      ) ;let
    ) ;define

    (define (%message-parts msg)
      (let* ((msg (%normalize-message msg))
             (role (%ref msg "role"))
             (content (%ref msg "content"))
             (text (pi-content->text content))
            ) ;
        (cond
         ((equal? role "user")
          (if (string-null? text) '() (list (string-append "[User]: " text)))
         ) ;
         ((equal? role "assistant") (%assistant-parts content text))
         ((equal? role "toolResult")
          (if (string-null? text)
            '()
            (list (string-append "[Tool result]: " (pi-truncate-for-summary text 2000)))
          ) ;if
         ) ;
         (else '())
        ) ;cond
      ) ;let*
    ) ;define

    (define (pi-serialize-conversation msgs)
      (string-join (append-map %message-parts msgs) "\n\n")
    ) ;define

  ) ;begin
) ;define-library
