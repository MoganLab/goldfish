;; gemini-stream-state-text-signature
;; 读取 Gemini 流状态中当前文本块的签名。
;;
;; 语法
;; ----
;; (gemini-stream-state-text-signature state)
;;
;; 参数
;; ----
;; state : gemini-stream-state
;; 流状态记录。
;;
;; 返回值
;; ----
;; string | #f
;; 当前文本块的签名；若无则为 #f。

(import (liii check) (liii pi ai api google-generative-ai stream-state))

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "" #f)))
  (check (gemini-stream-state? state) => #t)
  (check (gemini-stream-state-text-signature state) => #f)
) ;let


(check-report)
