;; gemini-stream-state-thinking-signature-set!
;; 设置 Gemini 流状态中当前思考块的签名。
;;
;; 语法
;; ----
;; (gemini-stream-state-thinking-signature-set! state value)
;;
;; 参数
;; ----
;; state : gemini-stream-state
;; 流状态记录。
;; value : string | #f
;; 当前思考块的签名；没有活动思考块时为空字符串。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新流状态。


(import (liii check) (liii pi ai api google-generative-ai stream-state))

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (gemini-stream-state-thinking-signature-set! state "sig-1")
  (check (gemini-stream-state-thinking-signature state) => "sig-1")
) ;let


(check-report)
