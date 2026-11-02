;; make-anthropic-stream-state
;; 创建 Anthropic 流状态记录，保存流式过程中的消息块、文本、用量与当前块状态。
;;
;; 语法
;; ----
;; (make-anthropic-stream-state blocks last-text usage tool-calls stop-reason current-tool current-thinking)
;;
;; 参数
;; ----
;; blocks : list
;; 已完成的 pi 消息块列表。
;; last-text : string | #f
;; 当前正在累积的文本；没有活动文本块时为 #f。
;; usage : alist
;; 已统计的用量 alist。
;; tool-calls : list
;; 已完成的 pi-tool-call-block 列表。
;; stop-reason : string | #f
;; Anthropic 返回的原始 stop_reason。
;; current-tool : any
;; 当前正在进行的工具块状态；没有活动工具块时为 #f。
;; current-thinking : any
;; 当前正在进行的 thinking 块状态；没有活动 thinking 块时为 #f。
;;
;; 返回值
;; ----
;; anthropic-stream-state
;; 流状态记录。

(import (liii check)
  (liii pi ai api anthropic-messages stream-state)
) ;import

(check-set-mode! 'report-failed)

;; 新建的流状态各字段与入参一致
(let ((state (make-anthropic-stream-state '() #f '() '() "end_turn" #f #f)))
  (check (anthropic-stream-state? state) => #t)
  (check (anthropic-stream-state-blocks state) => '())
  (check (anthropic-stream-state-last-text state) => #f)
  (check (anthropic-stream-state-usage state) => '())
  (check (anthropic-stream-state-tool-calls state) => '())
  (check (anthropic-stream-state-stop-reason state) => "end_turn")
  (check (anthropic-stream-state-current-tool state) => #f)
  (check (anthropic-stream-state-current-thinking state) => #f)
) ;let*

;; 入参非默认值时同样被保留
(let ((state (make-anthropic-stream-state '(blk) "text" '(usage) '(tc) "tool_use" 'tool 'thinking)))
  (check (anthropic-stream-state-blocks state) => '(blk))
  (check (anthropic-stream-state-last-text state) => "text")
  (check (anthropic-stream-state-usage state) => '(usage))
  (check (anthropic-stream-state-tool-calls state) => '(tc))
  (check (anthropic-stream-state-stop-reason state) => "tool_use")
  (check (anthropic-stream-state-current-tool state) => 'tool)
  (check (anthropic-stream-state-current-thinking state) => 'thinking)
) ;let*

(check-report)
