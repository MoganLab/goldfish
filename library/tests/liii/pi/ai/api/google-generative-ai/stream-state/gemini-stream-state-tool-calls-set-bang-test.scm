;; gemini-stream-state-tool-calls-set!
;; 设置 Gemini 流状态中已完成的工具调用列表。
;;
;; 语法
;; ----
;; (gemini-stream-state-tool-calls-set! state value)
;;
;; 参数
;; ----
;; state : gemini-stream-state
;; 流状态记录。
;; value : list
;; 已完成的 pi-tool-call-block 列表。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新流状态。


(import (liii check) (liii pi ai api google-generative-ai stream-state))

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (gemini-stream-state-tool-calls-set! state '())
  (check (gemini-stream-state-tool-calls state) => '())
) ;let


(check-report)
