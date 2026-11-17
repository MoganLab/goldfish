;; anthropic-stream-state-stop-reason-set!
;; 设置 Anthropic 流状态中的停止原因。
;;
;; 语法
;; ----
;; (anthropic-stream-state-stop-reason-set! state value)
;;
;; 参数
;; ----
;; state : anthropic-stream-state
;; 流状态记录。
;; value : string | #f
;; Anthropic 返回的原始 stop_reason。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新流状态。


(import (liii check)
  (liii pi ai api anthropic-messages stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-anthropic-stream-state '() #f '() '() "end_turn" #f #f)))
  (anthropic-stream-state-stop-reason-set! state "tool_use")
  (check (anthropic-stream-state-stop-reason state) => "tool_use")
) ;let*


(check-report)
