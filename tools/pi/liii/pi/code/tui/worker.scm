;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code tui worker)
  ;; 注意：go worker 线程通过重新 import 库并求值 procedure-source 来执行，
  ;; 只能看到各库的导出绑定，因此 worker 调用链上的内部函数必须全部导出
  (export make-tui-emit pi-tui-agent-worker pi-tui-run-worker pi-tui-send-event!
    pi-cfg-with-thinking
  ) ;export
  (import (scheme base)
    (scheme write)
    (scheme process-context)
    (liii go)
    (liii list)
    (liii string)
    (liii pi abort)
    (liii pi code args)
    (liii pi code config)
    (liii pi ai retry)
    (liii pi code modes flow)
    (liii pi code modes output)
  ) ;import
  (begin

    ;; 安全带超时的事件发送，防止 channel 堵塞导致 worker 永久卡死
    (define (pi-tui-send-event! ch ev)
      (catch #t (lambda () (chan-send! ch ev 2000)) (lambda (t . a) #f))
    ) ;define

    ;; 将 agent 会话事件翻译为 TUI 卡片事件并投递到 event-chan（生产与测试共用）
    (define (make-tui-emit event-chan)
      (let ((call-args-map '()))
        (lambda (event-type . args)
          (cond
            ;; 记录工具开始执行时的参数
            ((eq? event-type 'tool_execution_start)
             (when (>= (length args) 3)
               (let ((id (car args)) (call-args (caddr args)))
                 (set! call-args-map (cons (cons id call-args) call-args-map))
               ) ;let
             ) ;when
            ) ;
            ;; 工具执行完成
            ((eq? event-type 'tool_execution_end)
             (let* ((id (if (pair? args) (car args) ""))
                    (name (if (> (length args) 1) (cadr args) "tool"))
                    (raw-res (if (> (length args) 2) (caddr args) ""))
                    (is-err? (if (> (length args) 3) (cadddr args) #f))
                    (call-args
                      (let ((entry (assoc id call-args-map)))
                        (if entry (cdr entry) (if (> (length args) 4) (list-ref args 4) '()))
                      ) ;let
                    ) ;call-args
                   ) ;
               (pi-tui-send-event! event-chan
                 (list 'tool_execution_end name call-args is-err? raw-res)
               ) ;pi-tui-send-event!
             ) ;let*
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
            ;; 任务全部结算完成：携带 aborted 标志（主线程据此上屏 Aborted 提示）
            ((eq? event-type 'agent_settled)
             (pi-tui-send-event! event-chan
               (list 'agent_settled (and (pair? args) (car args)))
             ) ;pi-tui-send-event!
            ) ;
            (else #t)
          ) ;cond
        ) ;lambda
      ) ;let
    ) ;define

    ;; 覆盖 cfg alist 的 thinking-level 项（会话内 shift+tab 切换后生效）
    (define (pi-cfg-with-thinking cfg tl)
      (cons (cons 'thinking-level tl)
        (filter
          (lambda (p) (not (eq? (car p) 'thinking-level)))
          cfg
        ) ;filter
      ) ;cons
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
          (pi-tui-send-event! event-chan (list 'agent_settled #f))
        ) ;lambda
      ) ;catch
    ) ;define

    ;; 顶层后台工作协程：并发循环消费 jobs-chan 任务并回传事件到 event-chan
    ;; 注意：go 绑定的 let 语法会“求值”绑定值，alist 会被当作函数调用而报错，
    ;; 因此 parsed/cfg 不能跨 go 边界传入，必须在 worker 会话内自行解析
    (define (pi-tui-agent-worker jobs-chan event-chan abort-chan)
      (let* ((parsed (parse-pi-args (cdr (command-line))))
             (cfg (augment-pi-config (resolve-pi-config parsed (get-environment-variables))
                    parsed
                  ) ;augment-pi-config
             ) ;cfg
            ) ;
        ;; 重试与 multikey 换 key 日志共用 retry logger 通道，转为界面卡片
        (set-pi-retry-logger! (lambda (msg) (pi-tui-send-event! event-chan (list 'retry msg)))
        ) ;set-pi-retry-logger!
        (let loop
          ()
          ;; job 消息为 (text thinking-level)：会话内切换的级别对下一条消息生效
          (let ((job (chan-recv! jobs-chan)))
            (when job
              ;; 新任务开始前 arm abort 通道：排空上一轮残留信号并复位标志，
              ;; 本轮任务期间流式回调 / turn 边界 / 工具边界经 pi-aborted? 探测
              (pi-abort-arm-chan! abort-chan)
              (pi-tui-run-worker parsed
                (pi-cfg-with-thinking cfg (cadr job))
                (car job)
                event-chan
              ) ;pi-tui-run-worker
              (loop)
            ) ;when
          ) ;let
        ) ;let
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
