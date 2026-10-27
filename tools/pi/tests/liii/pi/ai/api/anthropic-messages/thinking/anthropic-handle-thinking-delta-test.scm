(import (liii check) (liii pi ai api anthropic-messages thinking))

;; anthropic-handle-thinking-delta
;; 处理 Anthropic thinking 或 signature 增量，并更新当前 thinking 状态。
;;
;; 语法
;; ----
;; (anthropic-handle-thinking-delta delta idx stream-state on-ev)
;;
;; 参数
;; ----
;; delta : alist
;; 包含 thinking 或 signature 字段的 Anthropic 增量对象。
;; idx : integer
;; 当前内容块索引。
;; stream-state : anthropic-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; on-ev : procedure | #f
;; 接收 thinking_delta 事件的回调；为 #f 时不派发事件。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用追加 thinking 文本和签名；仅非空文本增量触发事件。
;;
;; 说明
;; ----
;; 没有活动 thinking 块时忽略增量，签名增量本身不产生 thinking_delta 事件。
;;
;; 示例
;; ----
;; (anthropic-handle-thinking-delta '(("thinking" . "Step 1")) 0 stream-state on-ev)

(check-set-mode! 'report-failed)

(let* ((stream-state (vector '() #f '((input . 10) (output . 5)) '() #f #f #f))
       (events '())
       (emit
         (lambda (ev) (set! events (append events (list ev))))
       ) ;emit
      ) ;
  (vector-set! stream-state 6 '((thinking . "") (signature . "") (index . 0)))

  (anthropic-handle-thinking-delta '(("type" . "thinking_delta")
                                     ("thinking" . "Step 1"))
    0
    stream-state
    emit
  ) ;anthropic-handle-thinking-delta
  (check (length events) => 1)
  (check (cdr (assoc 'type (car events))) => "thinking_delta")
  (check (cdr (assoc 'delta (car events))) => "Step 1")

  (anthropic-handle-thinking-delta '(("type" . "signature_delta")
                                     ("signature" . "sig123"))
    0
    stream-state
    emit
  ) ;anthropic-handle-thinking-delta
  (check (length events) => 1)
) ;let*

(check-report)
