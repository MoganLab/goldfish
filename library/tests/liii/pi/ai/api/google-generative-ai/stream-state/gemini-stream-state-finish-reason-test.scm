;; gemini-stream-state-finish-reason
;; 读取 Gemini 流状态中的停止原因。
;;
;; 语法
;; ----
;; (gemini-stream-state-finish-reason state)
;;
;; 参数
;; ----
;; state : gemini-stream-state
;; 流状态记录。
;;
;; 返回值
;; ----
;; string | #f
;; Gemini 返回的原始 finishReason。

(import (liii check) (liii pi ai api google-generative-ai stream-state))

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (check (gemini-stream-state? state) => #t)
  (check (gemini-stream-state-finish-reason state) => "STOP")
) ;let


(check-report)
