;; openai-stream-state-last-reasoning
;; 读取 OpenAI 流状态中当前正在累积的思考内容。
;;
;; 语法
;; ----
;; (openai-stream-state-last-reasoning state)
;;
;; 参数
;; ----
;; state : openai-stream-state
;; 流状态记录。
;;
;; 返回值
;; ----
;; string | #f
;; 当前累积的思考内容；没有活动思考内容时为 #f。

(import (liii check)
  (liii pi ai api openai-completions stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-openai-stream-state '() #f '() "stop" #f '() "reasoning text")))
  (check (openai-stream-state? state) => #t)
  (check (openai-stream-state-last-reasoning state) => "reasoning text")
) ;let



(check-report)
