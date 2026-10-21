;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi tui worker)
  ;; 注意：go worker 线程通过重新 import 库并求值 procedure-source 来执行，
  ;; 只能看到各库的导出绑定，因此 worker 调用链上的内部函数必须全部导出
  (export make-tui-emit pi-tui-agent-worker pi-tui-run-worker pi-tui-send-event!)
  (import (scheme base)
    (scheme write)
    (scheme process-context)
    (liii go)
    (liii string)
    (liii pi args)
    (liii pi config)
    (liii pi retry)
    (liii pi modes flow)
    (liii pi modes output)
  ) ;import
  (begin

    ;; 安全带超时的事件发送，防止 channel 堵塞导致 worker 永久卡死
    (define (pi-tui-send-event! ch ev)
      (catch #t (lambda () (chan-send! ch ev 2000)) (lambda (t . a) #f))
    ) ;define

    ;; 将 agent 会话事件翻译为 TUI 卡片事件并投递到 event-chan（生产与测试共用）
    (define (make-tui-emit event-chan)
      (lambda (event-type . args)
        (cond
          ;; 工具执行完成
          ((eq? event-type 'tool_execution_end)
           (let ((name (if (> (length args) 1) (cadr args) "tool"))
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
                    (role (pi-message-field msg 'role ""))
                    (content (pi-message-field msg 'content '()))
                    (text (pi-content-text content))
                   ) ;
               (when
                 (and (equal? role "assistant") (not (string-null? (string-trim-both text))))
                 (pi-tui-send-event! event-chan (list 'assistant_text text))
               ) ;when
             ) ;let*
           ) ;when
          ) ;
          ;; 重试提示，转为卡片而非污染终端
          ((eq? event-type 'retry)
           (when (pair? args)
             (pi-tui-send-event! event-chan (list 'retry (car args)))
           ) ;when
          ) ;
          ;; 任务全部结算完成
          ((eq? event-type 'agent_settled)
           (pi-tui-send-event! event-chan (list 'agent_settled))
          ) ;
          (else #t)
        ) ;cond
      ) ;lambda
    ) ;define

    ;; 在 (liii go) 的后台独立 worker 线程中运行，完全不阻塞前台终端编辑与打字
    (define (pi-tui-run-worker parsed cfg user-text event-chan)
      (catch #t
        (lambda ()
          (run-session-flow parsed
            cfg
            user-text
            (make-tui-emit event-chan)
            (lambda (res-msgs) #t)
          ) ;run-session-flow
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
    ) ;define

    ;; 顶层后台工作协程：并发循环消费 jobs-chan 任务并回传事件到 event-chan
    ;; 注意：go 绑定的 let 语法会"求值"绑定值，alist 会被当作函数调用而报错，
    ;; 因此 parsed/cfg 不能跨 go 边界传入，必须在 worker 会话内自行解析
    (define (pi-tui-agent-worker jobs-chan event-chan)
      (let* ((parsed (parse-pi-args (cdr (command-line))))
             (cfg (resolve-pi-config parsed (get-environment-variables)))
            ) ;
        (set-pi-retry-logger! (lambda (msg) (pi-tui-send-event! event-chan (list 'retry msg)))
        ) ;set-pi-retry-logger!
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
