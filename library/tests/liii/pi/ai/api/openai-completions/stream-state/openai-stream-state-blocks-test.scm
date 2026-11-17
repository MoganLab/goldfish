;; openai-stream-state-blocks
;; 读取 OpenAI 流状态中的消息块列表。
;;
;; 语法
;; ----
;; (openai-stream-state-blocks state)
;;
;; 参数
;; ----
;; state : openai-stream-state
;; 流状态记录。
;;
;; 返回值
;; ----
;; list
;; 已完成的 pi 消息块列表。

(import (liii check)
  (liii pi ai api openai-completions stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-openai-stream-state '() #f '() "stop" #f '())))
  (check (openai-stream-state? state) => #t)
  (check (openai-stream-state-blocks state) => '())
) ;let*


(check-report)
