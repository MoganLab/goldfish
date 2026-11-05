;; anthropic-start-thinking-block
;; 开始一个新的 Anthropic thinking 块，重置当前 thinking 状态并派发 thinking_start 事件。
;;
;; 语法
;; ----
;; (anthropic-start-thinking-block stream-state idx on-ev)
;;
;; 参数
;; ----
;; stream-state : anthropic-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; idx : integer
;; 当前内容块索引。
;; on-ev : procedure | #f
;; 接收 thinking_start 事件的回调；为 #f 时不派发事件。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用建立当前 thinking 状态并清空 last-text。
;;
;; 说明
;; ----
;; 新的 thinking 状态以空文本、空签名开始，后续增量由
;; anthropic-handle-thinking-delta 追加。

(import (liii check)
  (liii pi ai api anthropic-messages thinking)
  (liii pi ai api anthropic-messages stream-state)
) ;import

(check-set-mode! 'report-failed)

;; 派发 thinking_start 事件并重置 last-text
(let* ((stream-state (vector '() "old text" '((input . 10) (output . 5)) '() #f #f #f))
       (events '())
       (emit
         (lambda (ev) (set! events (append events (list ev))))
       ) ;emit
      ) ;
  (anthropic-start-thinking-block stream-state 0 emit)
  (check (length events) => 1)
  (check (cdr (assoc 'type (car events))) => "thinking_start")
  (check (cdr (assoc 'contentIndex (car events))) => 0)
  (check (cdr (assoc 'usage (car events))) => '((input . 10) (output . 5)))
  (check (anthropic-stream-state-last-text stream-state) => #f)
  (check-true (not (eq? (anthropic-stream-state-current-thinking stream-state) #f)))
) ;let*

;; 回调为 #f 时不派发事件，但仍建立 thinking 状态
(let ((stream-state (vector '() #f '() '() #f #f #f)))
  (anthropic-start-thinking-block stream-state 3 #f)
  (check-true (not (eq? (anthropic-stream-state-current-thinking stream-state) #f)))
) ;let*

(check-report)
