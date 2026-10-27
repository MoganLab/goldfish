(import (liii check) (liii pi ai api google-generative-ai thinking))

;; gemini-append-thinking-block!
;; 向 Gemini 流状态追加 thinking 文本和可选签名。
;;
;; 语法
;; ----
;; (gemini-append-thinking-block! stream-state t ts on-event)
;;
;; 参数
;; ----
;; stream-state : gemini-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; t : string
;; 本次追加的 thinking 文本。
;; ts : string | #f
;; thinking 签名；后续增量为 #f 时保留已有签名。
;; on-event : procedure | #f
;; 接收 thinking_start 和 thinking_delta 事件的回调。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用创建或更新当前 pi-thinking-block。
;;
;; 说明
;; ----
;; 空文本被忽略；开始 thinking 前会结束当前活动的文本块。
;;
;; 示例
;; ----
;; (gemini-append-thinking-block! stream-state "Thinking" "sig-1" on-event)

(check-set-mode! 'report-failed)

(let* ((stream-state (vector '() #f '((input . 10) (output . 5)) '() #f #f #f #f))
       (events '())
       (emit
         (lambda (ev) (set! events (append events (list ev))))
       ) ;emit
      ) ;
  (gemini-append-thinking-block! stream-state "Thinking 1" "sig-1" emit)
  (check (vector-ref stream-state 5) => "thinking")
  (check (vector-ref stream-state 6) => "Thinking 1")
  (check (length (vector-ref stream-state 0)) => 1)
  (check (length events) => 2)
  (check (cdr (assoc 'type (car events))) => "thinking_start")
  (check (cdr (assoc 'type (cadr events))) => "thinking_delta")

  (gemini-append-thinking-block! stream-state " and more" #f emit)
  (check (vector-ref stream-state 6) => "Thinking 1 and more")
  (check (length (vector-ref stream-state 0)) => 1)
  (check (length events) => 3)
  (check (cdr (assoc 'type (caddr events))) => "thinking_delta")
) ;let*

(check-report)
