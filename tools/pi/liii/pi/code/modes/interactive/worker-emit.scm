;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; worker 事件桥：安全发送助手 + 错误文本提取 + agent 会话事件到 TUI 卡片事件的翻译
(define-library (liii pi code modes interactive worker-emit)
  (export pi-tui-send-event! %assistant-error-text make-tui-emit)
  (import (scheme base) (liii go) (liii string) (liii pi code modes output))
  (begin

    ;; 安全带超时的事件发送，防止 channel 堵塞导致 worker 永久卡死
    (define (pi-tui-send-event! ch ev)
      (catch #t (lambda () (chan-send! ch ev 2000)) (lambda (t . a) #f))
    ) ;define

    ;; 错误结算辅助：stopReason=error 的 assistant 消息返回应上屏的错误文本
    ;; （errorMessage 缺省或空白时兜底 "Error"，对齐上游 errorMessage || "Error"），
    ;; 非 error 结算返回 #f
    (define (%assistant-error-text msg)
      (let* ((stop-reason (pi-message-field msg 'stopReason ""))
             (err-msg (pi-message-field msg 'errorMessage #f))
            ) ;
        (and (equal? stop-reason "error")
          (if
            (and (string? err-msg) (not (string-null? (string-trim-both err-msg))))
            err-msg
            "Error"
          ) ;if
        ) ;and
      ) ;let*
    ) ;define

    ;; 返回 (剩余缓冲 . 完整行列表)，由事件桥统一投递并更新闭包缓冲。
    (define (%take-thinking-lines text)
      (let ((newline-at (string-index text #\newline)))
        (if (not newline-at)
          (cons text '())
          (let ((rest (%take-thinking-lines (substring text (+ newline-at 1) (string-length text)))
                ) ;rest
               ) ;
            (cons (car rest) (cons (substring text 0 newline-at) (cdr rest)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%flush-thinking-lines! event-chan lines)
      (for-each (lambda (line)
                  (pi-tui-send-event! event-chan (list 'assistant_thinking_line line))
                ) ;lambda
        lines
      ) ;for-each
    ) ;define

    (define (%thinking-delta event-chan buffer ev)
      (let ((delta-entry (assoc 'delta ev)))
        (if (and delta-entry (string? (cdr delta-entry)))
          (let* ((split (%take-thinking-lines (string-append buffer (cdr delta-entry))))
                 (lines (cdr split))
                ) ;
            (%flush-thinking-lines! event-chan lines)
            (car split)
          ) ;let*
          buffer
        ) ;if
      ) ;let
    ) ;define

    (define (%thinking-end event-chan buffer)
      (unless (string-null? (string-trim-both buffer))
        (%flush-thinking-lines! event-chan (list buffer))
      ) ;unless
      ""
    ) ;define

    (define (%message-update event-chan buffer ev)
      (let* ((type-entry (assoc 'type ev)) (stream-type (and type-entry (cdr type-entry))))
        (cond ((equal? stream-type "thinking_delta") (%thinking-delta event-chan buffer ev))
              ((equal? stream-type "thinking_end") (%thinking-end event-chan buffer))
              (else buffer)
        ) ;cond
      ) ;let*
    ) ;define

    ;; 将 agent 会话事件翻译为 TUI 卡片事件并投递到 event-chan（生产与测试共用）
    (define (make-tui-emit event-chan)
      (let ((call-args-map '()) (thinking-line-buffer ""))
        (lambda (event-type . args)
          (cond
            ;; provider 流式 thinking：跨增量缓冲，完整行立即投递；块结束冲出残余行
            ((eq? event-type 'message_update)
             (when (>= (length args) 2)
               (set! thinking-line-buffer
                 (%message-update event-chan thinking-line-buffer (cadr args))
               ) ;set!
             ) ;when
            ) ;
            ;; 工具开始：立即创建 pending 卡片，并记录参数供结束事件查找
            ((eq? event-type 'tool_execution_start)
             (when (>= (length args) 3)
               (let ((id (car args)) (name (cadr args)) (call-args (caddr args)))
                 (set! call-args-map (cons (cons id call-args) call-args-map))
                 (pi-tui-send-event! event-chan (list 'tool_execution_start id name call-args))
               ) ;let
             ) ;when
            ) ;
            ;; 流式部分结果：保留调用 id，主线程据此更新原卡片
            ((eq? event-type 'tool_execution_update)
             (when (>= (length args) 4)
               (pi-tui-send-event! event-chan
                 (list 'tool_execution_update (car args) (cadr args) (caddr args) (cadddr args))
               ) ;pi-tui-send-event!
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
                        (if entry (cdr entry) '())
                      ) ;let
                    ) ;call-args
                    (duration-ms (and (> (length args) 4) (list-ref args 4)))
                   ) ;
               (pi-tui-send-event! event-chan
                 (list 'tool_execution_end id name call-args is-err? raw-res duration-ms)
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
                      (err-text (%assistant-error-text msg))
                     ) ;
                 (when
                   (and (equal? role "assistant") (not (string-null? (string-trim-both text))))
                   (pi-tui-send-event! event-chan (list 'assistant_text text))
                 ) ;when
                 ;; 错误结算（对齐上游 message_end 渲染 errorMessage）：
                 ;; stopReason=error 时转为 error 卡片事件，保证用户可见
                 (when (and (equal? role "assistant") err-text)
                   (pi-tui-send-event! event-chan (list 'error err-text))
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

  ) ;begin
) ;define-library
