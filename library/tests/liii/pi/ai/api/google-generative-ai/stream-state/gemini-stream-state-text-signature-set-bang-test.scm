;; gemini-stream-state-text-signature-set!
;; 设置 Gemini 流状态中当前文本块的签名。
;;
;; 语法
;; ----
;; (gemini-stream-state-text-signature-set! state value)
;;
;; 参数
;; ----
;; state : gemini-stream-state | vector
;; 流状态；record 为当前表示，vector 用于兼容旧调用。
;; value : string | #f
;; 当前文本块的签名。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新流状态。

;; 说明
;; ----
;; record 状态直接更新记录字段；vector 状态更新对应的下标（第 8 位）。

(import (liii check)
  (liii pi ai api google-generative-ai stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "" #f)))
  (gemini-stream-state-text-signature-set! state "text-sig-1")
  (check (gemini-stream-state-text-signature state) => "text-sig-1")
) ;let*

;; vector 状态（兼容旧调用）
(let ((state (vector '() #f '() '() "STOP" #f "" "" #f)))
  (gemini-stream-state-text-signature-set! state "text-sig-2")
  (check (gemini-stream-state-text-signature state) => "text-sig-2")
) ;let*

(check-report)
