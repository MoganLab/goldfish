;; openai-stream-state-error-message-set!
;; 设置 OpenAI 流状态中的错误信息。
;;
;; 语法
;; ----
;; (openai-stream-state-error-message-set! state value)
;;
;; 参数
;; ----
;; state : openai-stream-state
;; 流状态记录。
;; value : string | #f
;; 错误描述信息；无错误时为 #f。
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
  (openai-stream-state-error-message-set! state "boom")
  (check (openai-stream-state-error-message state) => "boom")
) ;let*


(check-report)
