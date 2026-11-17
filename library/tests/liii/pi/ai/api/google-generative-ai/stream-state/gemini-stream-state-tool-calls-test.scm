;; gemini-stream-state-tool-calls
;; 读取 Gemini 流状态中已完成的工具调用列表。
;;
;; 语法
;; ----
;; (gemini-stream-state-tool-calls state)
;;
;; 参数
;; ----
;; state : gemini-stream-state
;; 流状态记录。
;;
;; 返回值
;; ----
;; list
;; 已完成的 pi-tool-call-block 列表。

(import (liii check) (liii pi ai api google-generative-ai stream-state))

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (check (gemini-stream-state? state) => #t)
  (check (gemini-stream-state-tool-calls state) => '())
) ;let


(check-report)
