;; gemini-stream-stop-active-block!
;; 结束 Gemini 流中当前活动的文本或 thinking 块。
;;
;; 语法
;; ----
;; (gemini-stream-stop-active-block! stream-state on-event)
;;
;; 参数
;; ----
;; stream-state : gemini-stream-state
;; 可变流状态记录。
;; on-event : procedure | #f
;; 接收 text_end 或 thinking_end 事件的回调。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用清除活动类型及对应的增量缓存。
;;
;; 说明
;; ----
;; 没有活动块时不执行任何操作；结束事件包含块索引、完整内容和 usage。

(import (liii check)
  (liii pi ai api google-generative-ai thinking)
  (liii pi ai api google-generative-ai stream-state)
) ;import

(check-set-mode! 'report-failed)

(let* ((stream-state (make-gemini-stream-state '() #f '((input . 10) (output . 5)) '() #f #f #f #f))
       (events '())
       (emit
         (lambda (ev) (set! events (append events (list ev))))
       ) ;emit
      ) ;
  (gemini-append-thinking-block! stream-state "Thinking 1" "sig-1" emit)
  (gemini-append-thinking-block! stream-state " and more" #f emit)

  (gemini-stream-stop-active-block! stream-state emit)
  (check (gemini-stream-state-current-active-type stream-state) => #f)
  (check (length events) => 4)
  (check (cdr (assoc 'type (list-ref events 3))) => "thinking_end")
  (check (cdr (assoc 'content (list-ref events 3))) => "Thinking 1 and more")
) ;let*

(check-report)
