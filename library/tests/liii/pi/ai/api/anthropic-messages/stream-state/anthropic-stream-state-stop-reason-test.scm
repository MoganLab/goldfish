;; anthropic-stream-state-stop-reason
;; 读取 Anthropic 流状态中的停止原因。
;;
;; 语法
;; ----
;; (anthropic-stream-state-stop-reason state)
;;
;; 参数
;; ----
;; state : anthropic-stream-state | vector
;; 流状态；record 为当前表示，vector 用于兼容旧调用。
;;
;; 返回值
;; ----
;; string | #f
;; Anthropic 返回的原始 stop_reason。

(import (liii check)
  (liii pi ai api anthropic-messages stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-anthropic-stream-state '() #f '() '() "end_turn" #f #f)))
  (check (anthropic-stream-state? state) => #t)
  (check (anthropic-stream-state-stop-reason state) => "end_turn")
) ;let*

;; vector 状态（兼容旧调用）
(let ((state (vector '() #f '() '() "end_turn" #f #f)))
  (check (anthropic-stream-state-stop-reason state) => "end_turn")
) ;let*

(check-report)
