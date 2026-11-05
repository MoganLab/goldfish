;; gemini-stream-state-text-signature
;; 读取 Gemini 流状态中当前文本块的签名。
;;
;; 语法
;; ----
;; (gemini-stream-state-text-signature state)
;;
;; 参数
;; ----
;; state : gemini-stream-state | vector
;; 流状态；record 为当前表示，vector 用于兼容旧调用。
;;
;; 返回值
;; ----
;; string | #f
;; 当前文本块的签名；若无则为 #f。

(import (liii check)
  (liii pi ai api google-generative-ai stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "" #f)))
  (check (gemini-stream-state? state) => #t)
  (check (gemini-stream-state-text-signature state) => #f)
) ;let*

;; vector 状态（兼容旧调用，长度 >= 9）
(let ((state (vector '() #f '() '() "STOP" #f "" "" "sig-9")))
  (check (gemini-stream-state-text-signature state) => "sig-9")
) ;let*

(check-report)
