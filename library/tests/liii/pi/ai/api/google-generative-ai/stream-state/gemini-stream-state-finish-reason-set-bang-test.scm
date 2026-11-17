;; gemini-stream-state-finish-reason-set!
;; 设置 Gemini 流状态中的停止原因。
;;
;; 语法
;; ----
;; (gemini-stream-state-finish-reason-set! state value)
;;
;; 参数
;; ----
;; state : gemini-stream-state
;; 流状态记录。
;; value : string | #f
;; Gemini 返回的原始 finishReason。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新流状态。


(import (liii check) (liii pi ai api google-generative-ai stream-state))

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (gemini-stream-state-finish-reason-set! state "MAX_TOKENS")
  (check (gemini-stream-state-finish-reason state) => "MAX_TOKENS")
) ;let


(check-report)
