;; gemini-stream-state-thinking-signature
;; 读取 Gemini 流状态中当前思考块的签名。
;;
;; 语法
;; ----
;; (gemini-stream-state-thinking-signature state)
;;
;; 参数
;; ----
;; state : gemini-stream-state
;; 流状态记录。
;;
;; 返回值
;; ----
;; string | #f
;; 当前思考块的签名；没有活动思考块时为空字符串。

(import (liii check) (liii pi ai api google-generative-ai stream-state))

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (check (gemini-stream-state? state) => #t)
  (check (gemini-stream-state-thinking-signature state) => "")
) ;let


(check-report)
