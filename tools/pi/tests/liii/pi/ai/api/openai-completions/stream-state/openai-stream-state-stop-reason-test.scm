;; openai-stream-state-stop-reason
;; 读取 OpenAI 流状态中的停止原因。
;;
;; 语法
;; ----
;; (openai-stream-state-stop-reason state)
;;
;; 参数
;; ----
;; state : openai-stream-state | vector
;; 流状态；record 为当前表示，vector 用于兼容旧调用。
;;
;; 返回值
;; ----
;; string | #f
;; OpenAI 返回的原始 finish_reason 映射值。

(import (liii check)
  (liii pi ai api openai-completions stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-openai-stream-state '() #f '() "stop" #f '())))
  (check (openai-stream-state? state) => #t)
  (check (openai-stream-state-stop-reason state) => "stop")
) ;let*

;; vector 状态（兼容旧调用）
(let ((state (vector '() #f '() "stop" #f '())))
  (check (openai-stream-state-stop-reason state) => "stop")
) ;let*

(check-report)
