;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi tui handler)
  (export make-tui-agent-handler)
  (import (scheme base)
    (scheme write)
    (liii string)
    (liii list)
    (liii pi message)
    (liii pi agent queue)
    (liii pi tui app)
    (liii pi modes flow)
    (liii pi modes output)
  ) ;import
  (begin

    (define (%extract-text-from-content content)
      (let* ((blks (cond ((vector? content) (vector->list content))
                         ((list? content) content)
                         (else '())
                   ) ;cond
             ) ;blks
             (texts
               (map
                 (lambda (b)
                   (let* ((alist (cond ((pi-block? b) (pi-block->alist b)) ((pair? b) b) (else '())))
                          (type (cdr (or (assoc 'type alist) (assoc "type" alist) '(#f
                                                                                    . ""))))
                          (txt (cdr (or (assoc 'text alist) (assoc "text" alist) '(#f
                                                                                   . ""))))
                         ) ;
                     (if (and (equal? type "text") (string? txt)) txt "")
                   ) ;let*
                 ) ;lambda
                 blks
               ) ;map
             ) ;texts
            ) ;
        (apply string-append texts)
      ) ;let*
    ) ;define

    (define* (make-tui-agent-handler parsed cfg (runner run-session-flow))
      (let* ((provider (cdr (or (assoc 'provider cfg) '(#f . "google"))))
             (api-key (cdr (or (assoc 'api-key cfg) '(#f . #f))))
            ) ;
        (lambda (app user-text)
          ;; 1. 凭证检查
          (if (or (not api-key) (and (string? api-key) (string-null? api-key)))
            (begin
              (pi-app-add-tool-result! app
                "auth"
                '()
                'error
                (string-append "No credentials found for provider \"" provider "\"")
              ) ;pi-app-add-tool-result!
              (pi-queue-finish-task! (pi-app-queue app))
              (pi-app-drain-queue! app)
            ) ;begin
            ;; 2. 真实/模拟运行
            (let ((tui-emit
                    (lambda (event-type . args)
                      (cond
                        ;; 工具调用完成
                        ((eq? event-type 'tool_execution_end)
                         (let ((id (if (pair? args) (car args) ""))
                               (name (if (> (length args) 1) (cadr args) "tool"))
                               (raw-res (if (> (length args) 2) (caddr args) ""))
                               (is-err? (if (> (length args) 3) (cadddr args) #f))
                              ) ;
                           (pi-app-add-tool-result! app name '() (if is-err? 'error 'success) raw-res)
                         ) ;let
                        ) ;
                        ;; 消息回复
                        ((eq? event-type 'message_end)
                         (when (pair? args)
                           (let* ((msg (car args))
                                  (alist (cond ((pi-message? msg) (pi-message->alist msg))
                                               ((pair? msg) msg)
                                               (else '())
                                         ) ;cond
                                  ) ;alist
                                  (role (cdr (or (assoc 'role alist) (assoc "role" alist) '(#f
                                                                                            . ""))))
                                  (content (cdr (or (assoc 'content alist) (assoc "content" alist) '(#f
                                                                                                     #_quote
                                                                                                     ())))
                                  ) ;content
                                  (text (%extract-text-from-content content))
                                 ) ;
                             (when
                               (and (equal? role "assistant") (not (string-null? (string-trim-both text))))
                               (pi-app-add-assistant-text! app text)
                             ) ;when
                           ) ;let*
                         ) ;when
                        ) ;
                        ;; 任务结算
                        ((eq? event-type 'agent_settled)
                         (pi-queue-finish-task! (pi-app-queue app))
                         (pi-app-drain-queue! app)
                        ) ;
                        (else #t)
                      ) ;cond
                    ) ;lambda
                  ) ;tui-emit
                 ) ;
              (catch #t
                (lambda () (runner parsed cfg user-text tui-emit (lambda (res-msgs) #t)))
                (lambda (tag . err-args)
                  (pi-app-add-tool-result! app
                    "error"
                    '()
                    'error
                    (string-append (object->string tag) ": " (object->string err-args))
                  ) ;pi-app-add-tool-result!
                  (pi-queue-finish-task! (pi-app-queue app))
                  (pi-app-drain-queue! app)
                ) ;lambda
              ) ;catch
            ) ;let
          ) ;if
        ) ;lambda
      ) ;let*
    ) ;define*
  ) ;begin

) ;define-library
