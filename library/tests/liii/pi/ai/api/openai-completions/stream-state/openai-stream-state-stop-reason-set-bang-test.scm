;; openai-stream-state-stop-reason-set!
;; 设置 OpenAI 流状态中的停止原因。
;;
;; 语法
;; ----
;; (openai-stream-state-stop-reason-set! state value)
;;
;; 参数
;; ----
;; state : openai-stream-state
;; 流状态记录。
;; value : string | #f
;; OpenAI 返回的原始 finish_reason 映射值。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新流状态。


(import (liii check)
  (liii pi ai api openai-completions stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-openai-stream-state '() #f '() "stop" #f '())))
  (openai-stream-state-stop-reason-set! state "toolUse")
  (check (openai-stream-state-stop-reason state) => "toolUse")
) ;let*


(check-report)
