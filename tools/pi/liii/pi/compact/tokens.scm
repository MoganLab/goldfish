;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compact tokens)
  (import (scheme base) (scheme write) (liii string) (liii pi message))
  (export pi-estimate-content-chars pi-estimate-message-tokens
    pi-context-token-estimate pi-should-compact? pi-compaction-default-settings
  ) ;export
  (begin

    (define (pi-compaction-default-settings)
      (list (cons 'enabled #t)
        (cons 'reserve-tokens 16384)
        (cons 'keep-recent-tokens 20000)
      ) ;list
    ) ;define

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

    (define (%block-chars blk)
      (cond ((pi-block? blk) (%block-chars (pi-block->alist blk)))
            ((not (pair? blk)) 0)
            (else
              (let ((type (%ref blk "type")))
                (cond
                 ((equal? type "text")
                  (let ((t (%ref blk "text")))
                    (if (string? t) (string-length t) 0)
                  ) ;let
                 ) ;
                 ((equal? type "thinking")
                  (let ((t (%ref blk "thinking")))
                    (if (string? t) (string-length t) 0)
                  ) ;let
                 ) ;
                 ((equal? type "toolCall")
                  (let ((name (%ref blk "name")) (args (%ref blk "arguments")))
                    (+ (if (string? name) (string-length name) 0)
                      (if args (string-length (object->string args)) 0)
                    ) ;+
                  ) ;let
                 ) ;
                 (else 0)
                ) ;cond
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (pi-estimate-content-chars content)
      (cond ((string? content) (string-length content))
            ((vector? content) (pi-estimate-content-chars (vector->list content)))
            ((null? content) 0)
            ((pair? content)
             (if
               (and (pair? (car content))
                 (or (string? (caar content)) (symbol? (caar content)))
               ) ;and
               (%block-chars content)
               (let loop
                 ((rest content) (acc 0))
                 (if (null? rest) acc (loop (cdr rest) (+ acc (%block-chars (car rest)))))
               ) ;let
             ) ;if
            ) ;
            (else 0)
      ) ;cond
    ) ;define

    (define (pi-estimate-message-tokens msg)
      (let* ((m (%normalize-message msg))
             (role (%ref m "role"))
             (content (%ref m "content"))
             (chars
               (if (equal? role "assistant")
                 (if (string? content)
                   (pi-estimate-content-chars content)
                   (let loop
                     ((rest (cond ((vector? content) (vector->list content))
                                  ((list? content) content)
                                  (else '())
                            ) ;cond
                      ) ;rest
                      (acc 0)
                     ) ;
                     (if (null? rest) acc (loop (cdr rest) (+ acc (%block-chars (car rest)))))
                   ) ;let
                 ) ;if
                 (pi-estimate-content-chars content)
               ) ;if
             ) ;chars
            ) ;
        (quotient (+ chars 3) 4)
      ) ;let*
    ) ;define

    (define (%usage-total usage)
      (let ((total (%ref usage "totalTokens"))
            (total-alt (%ref usage "total"))
            (in (%ref usage "input"))
            (out (%ref usage "output"))
            (cr (%ref usage "cacheRead"))
            (cw (%ref usage "cacheWrite"))
           ) ;
        (cond ((and (number? total) (> total 0)) total)
              ((and (number? total-alt) (> total-alt 0)) total-alt)
              (else (+ (if (number? in) in 0)
                      (if (number? out) out 0)
                      (if (number? cr) cr 0)
                      (if (number? cw) cw 0)
                    ) ;+
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (%valid-usage? m)
      (and (equal? (%ref m "role") "assistant")
        (not (equal? (%ref m "stopReason") "error"))
        (not (equal? (%ref m "stopReason") "aborted"))
        (let ((u (%ref m "usage")))
          (and (pair? u) (> (%usage-total u) 0))
        ) ;let
      ) ;and
    ) ;define

    (define (pi-context-token-estimate msgs)
      (let ((norm (map %normalize-message msgs)))
        (let loop
          ((rev (reverse norm)) (idx (- (length norm) 1)) (trailing 0))
          (cond ((null? rev)
                 (list (cons 'tokens trailing)
                   (cons 'usage-tokens 0)
                   (cons 'trailing-tokens trailing)
                 ) ;list
                ) ;
                ((%valid-usage? (car rev))
                 (let ((u (%usage-total (%ref (car rev) "usage"))))
                   (list (cons 'tokens (+ u trailing))
                     (cons 'usage-tokens u)
                     (cons 'trailing-tokens trailing)
                   ) ;list
                 ) ;let
                ) ;
                (else
                  (loop (cdr rev) (- idx 1) (+ trailing (pi-estimate-message-tokens (car rev))))
                ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (%setting settings key default)
      (let ((pair
              (or (assoc key settings)
                (assoc (string->symbol (if (symbol? key) (symbol->string key) key)) settings)
              ) ;or
            ) ;pair
           ) ;
        (if pair (cdr pair) default)
      ) ;let
    ) ;define

    (define (pi-should-compact? context-tokens context-window settings)
      (and (number? context-window)
        (> context-window 0)
        (eq? #t (%setting settings 'enabled #t))
        (> context-tokens (- context-window (%setting settings 'reserve-tokens 16384)))
      ) ;and
    ) ;define

  ) ;begin
) ;define-library
