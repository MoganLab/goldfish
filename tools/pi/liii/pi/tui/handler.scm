;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi tui handler)
  (export make-tui-agent-handler)
  (import (scheme base)
    (scheme write)
    (liii go)
    (liii string)
    (liii list)
    (liii pi message)
    (liii pi agent queue)
    (liii pi tui app)
    (liii pi tui worker)
    (liii pi modes flow)
    (liii pi modes output)
  ) ;import
  (begin

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
            (let ((ch (pi-app-event-chan app)))
              (if (eq? runner run-session-flow)
                ;; 生产分支：通过 (liii go) 启动后台独立 Worker 线程，显式 import 依赖库，带超时防护！
                (begin
                  (g_go-spawn (list 'g_parsed 'g_cfg 'g_text 'g_chan)
                    (list parsed cfg user-text ch)
                    `(begin
                       (set! *load-path* (quote ,*load-path*))
                       (import (scheme base)
                         (liii go)
                         (liii string)
                         (liii list)
                         (liii pi message)
                         (liii pi modes flow)
                         (liii pi tui worker))
                       (pi-tui-run-worker g_parsed g_cfg g_text g_chan))
                  ) ;g_go-spawn
                  'async
                ) ;begin
                ;; 测试分支：使用传入的 mock runner 同步执行并派发到 event-chan
                (let ((tui-emit
                        (lambda (event-type . args)
                          (cond
                           ((eq? event-type 'tool_execution_end)
                            (let ((id (if (pair? args) (car args) ""))
                                  (name (if (> (length args) 1) (cadr args) "tool"))
                                  (raw-res (if (> (length args) 2) (caddr args) ""))
                                  (is-err? (if (> (length args) 3) (cadddr args) #f))
                                 ) ;
                              (pi-tui-send-event! ch (list 'tool_execution_end name '() is-err? raw-res))
                            ) ;let
                           ) ;
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
                                     (text (pi-extract-text-from-content content))
                                    ) ;
                                (when
                                  (and (equal? role "assistant") (not (string-null? (string-trim-both text))))
                                  (pi-tui-send-event! ch (list 'assistant_text text))
                                ) ;when
                              ) ;let*
                            ) ;when
                           ) ;
                            ((eq? event-type 'retry)
                             (pi-tui-send-event! ch (list 'retry (car args)))
                            ) ;
                            ((eq? event-type 'agent_settled) (pi-tui-send-event! ch (list 'agent_settled)))
                           (else #t)
                          ) ;cond
                        ) ;lambda
                      ) ;tui-emit
                     ) ;
                  (runner parsed cfg user-text tui-emit (lambda (res-msgs) #t))
                  (pi-app-poll-events! app)
                ) ;let
              ) ;if
            ) ;let
          ) ;if
        ) ;lambda
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
