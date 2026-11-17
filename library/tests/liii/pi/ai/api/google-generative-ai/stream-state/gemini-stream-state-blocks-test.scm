;; gemini-stream-state-blocks
;; 读取 Gemini 流状态中的消息块列表。
;;
;; 语法
;; ----
;; (gemini-stream-state-blocks state)
;;
;; 参数
;; ----
;; state : gemini-stream-state
;; 流状态记录。
;;
;; 返回值
;; ----
;; list
;; 已完成的 pi 消息块列表。

(import (liii check) (liii pi ai api google-generative-ai stream-state))

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (check (gemini-stream-state? state) => #t)
  (check (gemini-stream-state-blocks state) => '())
) ;let


(check-report)
