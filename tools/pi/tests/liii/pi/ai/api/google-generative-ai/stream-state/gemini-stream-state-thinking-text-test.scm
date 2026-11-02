;; gemini-stream-state-thinking-text
;; 读取 Gemini 流状态中当前正在累积的思考文本。
;;
;; 语法
;; ----
;; (gemini-stream-state-thinking-text state)
;;
;; 参数
;; ----
;; state : gemini-stream-state | vector
;; 流状态；record 为当前表示，vector 用于兼容旧调用。
;;
;; 返回值
;; ----
;; string | #f
;; 当前累积的思考文本；没有活动思考块时为空字符串。

(import (liii check)
  (liii pi ai api google-generative-ai stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (check (gemini-stream-state? state) => #t)
  (check (gemini-stream-state-thinking-text state) => "")
) ;let*

;; vector 状态（兼容旧调用）
(let ((state (vector '() #f '() '() "STOP" #f "" "")))
  (check (gemini-stream-state-thinking-text state) => "")
) ;let*

(check-report)
