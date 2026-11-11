;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 跨模型交接前回滚尾部不可用 assistant，保留完整消息和已执行工具结果。
(define-library (liii pi code all-in-one boundary)
  (import (scheme base) (liii string) (liii pi ai message))
  (export pi-boundary-recover-messages pi-boundary-message-role
    pi-boundary-message-stop-reason pi-boundary-message-content
    pi-boundary-assistant-usable?
  ) ;export
  (begin

    (define (%field obj key default)
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

    (define (pi-boundary-message-role msg)
      (cond ((pi-message? msg) (pi-message-role msg))
            ((pair? msg) (%field msg "role" #f))
            (else #f)
      ) ;cond
    ) ;define

    (define (pi-boundary-message-stop-reason msg)
      (cond ((pi-assistant-message? msg) (pi-assistant-message-stop-reason msg))
            ((pair? msg) (%field msg "stopReason" #f))
            (else #f)
      ) ;cond
    ) ;define

    (define (pi-boundary-message-content msg)
      (let ((raw (cond ((pi-user-message? msg) (pi-user-message-content msg))
                       ((pi-assistant-message? msg) (pi-assistant-message-content msg))
                       ((pi-tool-result-message? msg) (pi-tool-result-message-content msg))
                       ((pair? msg) (%field msg "content" '()))
                       (else '())
                 ) ;cond
            ) ;raw
           ) ;
        (cond ((vector? raw) (vector->list raw))
              ((pair? raw) raw)
              ((string? raw) (list raw))
              (else '())
        ) ;cond
      ) ;let
    ) ;define

    ;; 错误、中止、截断、空内容和未执行的 toolUse 均不可交接。
    (define (pi-boundary-assistant-usable? msg)
      (let ((error (cond ((pi-assistant-message? msg) (pi-assistant-message-error-message msg))
                         ((pair? msg) (%field msg "errorMessage" #f))
                         (else #f)
                   ) ;cond
            ) ;error
           ) ;
        (and
          (not
            (and (string? error) (not (string-null? (string-trim-both error))))
          ) ;not
          (not (member (pi-boundary-message-stop-reason msg)
                 '("error" "aborted" "length" "toolUse")
               ) ;member
          ) ;not
          (pair? (pi-boundary-message-content msg))
        ) ;and
      ) ;let
    ) ;define

    (define (pi-boundary-recover-messages messages)
      (let loop
        ((msgs (if (list? messages) messages '())))
        (if (null? msgs)
          '()
          (let ((tail (car (reverse msgs))))
            (if (and (equal? (pi-boundary-message-role tail) "assistant")
                  (not (pi-boundary-assistant-usable? tail))
                ) ;and
              (loop (reverse (cdr (reverse msgs))))
              msgs
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
