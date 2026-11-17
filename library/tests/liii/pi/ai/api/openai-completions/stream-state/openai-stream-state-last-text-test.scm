;; openai-stream-state-last-text
;; 读取 OpenAI 流状态中当前正在累积的文本。
;;
;; 语法
;; ----
;; (openai-stream-state-last-text state)
;;
;; 参数
;; ----
;; state : openai-stream-state
;; 流状态记录。
;;
;; 返回值
;; ----
;; string | #f
;; 当前累积的文本；没有活动文本块时为 #f。

(import (liii check)
  (liii pi ai api openai-completions stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-openai-stream-state '() "partial" '() "stop" #f '())))
  (check (openai-stream-state? state) => #t)
  (check (openai-stream-state-last-text state) => "partial")
) ;let*


(check-report)
