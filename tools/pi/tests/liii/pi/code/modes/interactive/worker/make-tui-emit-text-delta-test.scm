;; make-tui-emit-text-delta
;; 验证正文流式增量与 message_end 的协作去重与块结束信号。
;;
;; 语法
;; ----
;; (make-tui-emit event-chan)
;;
;; 参数
;; ----
;; event-chan : chan?
;; 事件通道。
;;
;; 返回值
;; ----
;; procedure?
;; 返回事件发射函数。

(import (scheme base)
  (liii check)
  (liii go)
  (liii pi code modes interactive worker-emit)
) ;import

(check-set-mode! 'report-failed)

(define (recv-ev! ch)
  (chan-try-recv! ch 'none)
) ;define

(define (stream-event type . fields)
  (cons (cons 'type type) fields)
) ;define

(define (assistant-msg text)
  (list (cons 'role "assistant")
    (cons 'content (list (list (cons 'type "text") (cons 'text text))))
    (cons 'stopReason "stop")
  ) ;list
) ;define

;; 1. 流式 text_delta 即时投递；text_end 发结束信号；message_end 不重复投递 assistant_text
(let* ((ch (make-chan 16)) (emit (make-tui-emit ch)))
  (emit 'message_update '() (stream-event "text_delta" '(delta . "你")))
  (check (recv-ev! ch) => '(assistant_text_delta "你"))
  (emit 'message_update '() (stream-event "text_delta" '(delta . "好，世界")))
  (check (recv-ev! ch) => '(assistant_text_delta "好，世界"))
  (emit 'message_update '() (stream-event "text_end"))
  (check (recv-ev! ch) => '(assistant_text_end))
  (emit 'message_end (assistant-msg "你好，世界"))
  ;; message_end 补发幂等的 assistant_text_end，但不再投递整段 assistant_text
  (check (recv-ev! ch) => '(assistant_text_end))
  (check (recv-ev! ch) => 'none)
) ;let*

;; 2. provider 未显式发送 text_end（如 OpenAI Completions）时，message_end 自动补发结束信号
(let* ((ch (make-chan 16)) (emit (make-tui-emit ch)))
  (emit 'message_update '() (stream-event "text_delta" '(delta . "hello")))
  (check (recv-ev! ch) => '(assistant_text_delta "hello"))
  (emit 'message_end (assistant-msg "hello"))
  (check (recv-ev! ch) => '(assistant_text_end))
  (check (recv-ev! ch) => 'none)

  ;; 下一轮非流式 message_end 仍可兜底投递 assistant_text
  (emit 'message_end (assistant-msg "second turn"))
  (check (recv-ev! ch) => '(assistant_text "second turn"))
  (check (recv-ev! ch) => 'none)
) ;let*

;; 3. 仅有纯空白增量时，message_end 不跳过兜底（若正文非空仍可上屏），且空 delta 被忽略
(let* ((ch (make-chan 16)) (emit (make-tui-emit ch)))
  (emit 'message_update '() (stream-event "text_delta" '(delta . "")))
  (check (recv-ev! ch) => 'none)
  (emit 'message_update '() (stream-event "text_delta" '(delta . "\n")))
  (check (recv-ev! ch) => '(assistant_text_delta "\n"))
  (emit 'message_end (assistant-msg "ok"))
  (check (recv-ev! ch) => '(assistant_text "ok"))
  (check (recv-ev! ch) => 'none)
) ;let*

(check-report)
