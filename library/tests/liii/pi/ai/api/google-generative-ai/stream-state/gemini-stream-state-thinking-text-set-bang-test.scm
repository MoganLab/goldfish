;; gemini-stream-state-thinking-text-set!
;; 设置 Gemini 流状态中当前正在累积的思考文本。
;;
;; 语法
;; ----
;; (gemini-stream-state-thinking-text-set! state value)
;;
;; 参数
;; ----
;; state : gemini-stream-state
;; 流状态记录。
;; value : string | #f
;; 当前累积的思考文本；没有活动思考块时为空字符串。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新流状态。


(import (liii check) (liii pi ai api google-generative-ai stream-state))

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (gemini-stream-state-thinking-text-set! state "thinking...")
  (check (gemini-stream-state-thinking-text state) => "thinking...")
) ;let


(check-report)
