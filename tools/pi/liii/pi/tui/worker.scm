;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi tui worker)
  (export pi-tui-run-worker pi-extract-text-from-content pi-tui-send-event!
    pi-tui-agent-worker
  ) ;export
  (import (scheme base)
    (scheme write)
    (scheme process-context)
    (liii go)
    (liii string)
    (liii list)
    (liii pi config)
    (liii pi args)
    (liii pi retry)
    (liii pi message)
    (liii pi modes flow)
    (liii pi modes output)
  ) ;import
  (begin

    (define (pi-extract-text-from-content content)
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

    ;; 安全带超时的事件发送，防止 channel 堵塞导致 worker 永久卡死
    (define (pi-tui-send-event! ch ev)
      (catch #t (lambda () (chan-send! ch ev 2000)) (lambda (t . a) #f))
    ) ;define

    ;; 在 (liii go) 的后台独立 worker 线程中运行，完全不阻塞前台终端编辑与打字
    (define (pi-tui-run-worker parsed cfg user-text event-chan)
      (let ((tui-emit
              (lambda (event-type . args)
                (cond
                  ;; 工具执行完成
                  ((eq? event-type 'tool_execution_end)
                   (let ((id (if (pair? args) (car args) ""))
                         (name (if (> (length args) 1) (cadr args) "tool"))
                         (raw-res (if (> (length args) 2) (caddr args) ""))
                         (is-err? (if (> (length args) 3) (cadddr args) #f))
                        ) ;
                     (pi-tui-send-event! event-chan
                       (list 'tool_execution_end name '() is-err? raw-res)
                     ) ;pi-tui-send-event!
                   ) ;let
                  ) ;
                  ;; 消息回复完成
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
                         (pi-tui-send-event! event-chan (list 'assistant_text text))
                       ) ;when
                     ) ;let*
                   ) ;when
                  ) ;
                  ;; 任务全部结算完成
                  ((eq? event-type 'agent_settled)
                   (pi-tui-send-event! event-chan (list 'agent_settled))
                  ) ;
                  (else #t)
                ) ;cond
              ) ;lambda
            ) ;tui-emit
           ) ;
        (set-pi-retry-logger!
          (lambda (msg)
            (pi-tui-send-event! event-chan (list 'retry msg))
          ) ;lambda
        ) ;set-pi-retry-logger!
        (catch #t
          (lambda ()
            (run-session-flow parsed cfg user-text tui-emit (lambda (res-msgs) #t))
          ) ;lambda
          (lambda (tag . err-args)
            (pi-tui-send-event! event-chan
              (list 'error
                (string-append (object->string tag) ": " (object->string err-args))
              ) ;list
            ) ;pi-tui-send-event!
            (pi-tui-send-event! event-chan (list 'agent_settled))
          ) ;lambda
        ) ;catch
      ) ;let
    ) ;define

    ;; 顶层后台工作协程：并发循环消费 jobs-chan 任务并回传事件到 event-chan
    (define (pi-tui-agent-worker jobs-chan event-chan)
      (let* ((env (get-environment-variables))
             (raw-args (command-line))
             (parsed (parse-pi-args (if (pair? raw-args) (cdr raw-args) '())))
             (cfg (resolve-pi-config parsed env))
            ) ;
        (let loop
          ()
          (let ((text (chan-recv! jobs-chan)))
            (when text
              (pi-tui-run-worker parsed cfg text event-chan)
              (loop)
            ) ;when
          ) ;let
        ) ;let
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
