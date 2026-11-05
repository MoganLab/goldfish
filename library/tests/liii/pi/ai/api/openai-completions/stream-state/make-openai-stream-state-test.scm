;; make-openai-stream-state
;; 创建 OpenAI 流状态记录，保存流式过程中的消息块、文本、用量与当前块状态。
;;
;; 语法
;; ----
;; (make-openai-stream-state blocks last-text usage stop-reason error-message tools)
;;
;; 参数
;; ----
;; blocks : list
;; 已完成的 pi 消息块列表。
;; last-text : string | #f
;; 当前正在累积的文本；没有活动文本块时为 #f。
;; usage : alist
;; 已统计的用量 alist。
;; stop-reason : string | #f
;; 原始 finish_reason 的映射值。
;; error-message : string | #f
;; 错误描述信息；无错误时为 #f。
;; tools : list
;; 当前正在拼接的 tool_call_state 关联表。
;;
;; 返回值
;; ----
;; openai-stream-state
;; 流状态记录。

(import (liii check)
  (liii pi ai api openai-completions stream-state)
) ;import

(check-set-mode! 'report-failed)

;; 新建的流状态各字段与入参一致
(let ((state (make-openai-stream-state '() #f '() "stop" #f '())))
  (check (openai-stream-state? state) => #t)
  (check (openai-stream-state-blocks state) => '())
  (check (openai-stream-state-last-text state) => #f)
  (check (openai-stream-state-usage state) => '())
  (check (openai-stream-state-stop-reason state) => "stop")
  (check (openai-stream-state-error-message state) => #f)
  (check (openai-stream-state-tools state) => '())
) ;let*

;; 入参非默认值时同样被保留
(let ((state (make-openai-stream-state '(blk) "text" '(usage) "length" "err" '(tc))))
  (check (openai-stream-state-blocks state) => '(blk))
  (check (openai-stream-state-last-text state) => "text")
  (check (openai-stream-state-usage state) => '(usage))
  (check (openai-stream-state-stop-reason state) => "length")
  (check (openai-stream-state-error-message state) => "err")
  (check (openai-stream-state-tools state) => '(tc))
) ;let*

(check-report)
