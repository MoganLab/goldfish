;; openai-stream-state-error-message
;; 读取 OpenAI 流状态中的错误信息。
;;
;; 语法
;; ----
;; (openai-stream-state-error-message state)
;;
;; 参数
;; ----
;; state : openai-stream-state | vector
;; 流状态；record 为当前表示，vector 用于兼容旧调用。
;;
;; 返回值
;; ----
;; string | #f
;; 错误描述信息；无错误时为 #f。

(import (liii check)
  (liii pi ai api openai-completions stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-openai-stream-state '() #f '() "stop" #f '())))
  (check (openai-stream-state? state) => #t)
  (check (openai-stream-state-error-message state) => #f)
) ;let*

;; vector 状态（兼容旧调用）
(let ((state (vector '() #f '() "stop" #f '())))
  (check (openai-stream-state-error-message state) => #f)
) ;let*

(check-report)
