(import (liii check)
  (liii pi ai message)
  (liii pi ai api anthropic-messages thinking)
) ;import

;; anthropic-finish-thinking-block
;; 结束当前 Anthropic thinking 块，将累积内容写入消息块列表。
;;
;; 语法
;; ----
;; (anthropic-finish-thinking-block stream-state on-ev)
;;
;; 参数
;; ----
;; stream-state : anthropic-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; on-ev : procedure | #f
;; 接收 thinking_end 事件的回调；为 #f 时不派发事件。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用追加 pi-thinking-block 并清除当前 thinking 状态。
;;
;; 说明
;; ----
;; 空签名转换为 #f；没有活动 thinking 块时不执行任何操作。
;;
;; 示例
;; ----
;; (anthropic-finish-thinking-block stream-state on-ev)

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
  (anthropic-handle-thinking-delta '(("type" . "signature_delta")
                                     ("signature" . "sig123"))
    0
    stream-state
    emit
  ) ;anthropic-handle-thinking-delta

  (anthropic-finish-thinking-block stream-state emit)
  (check (length events) => 2)
  (check (cdr (assoc 'type (cadr events))) => "thinking_end")
  (check (cdr (assoc 'content (cadr events))) => "Step 1")
  (check (length (vector-ref stream-state 0)) => 1)
  (let ((blk (car (vector-ref stream-state 0))))
    (check-true (pi-thinking-block? blk))
    (check (pi-thinking-block-thinking blk) => "Step 1")
    (check (pi-thinking-block-thinking-signature blk) => "sig123")
  ) ;let
) ;let*

(check-report)
