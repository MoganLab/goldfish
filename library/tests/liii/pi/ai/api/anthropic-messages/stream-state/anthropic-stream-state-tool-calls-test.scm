;; anthropic-stream-state-tool-calls
;; 读取 Anthropic 流状态中已完成的工具调用列表。
;;
;; 语法
;; ----
;; (anthropic-stream-state-tool-calls state)
;;
;; 参数
;; ----
;; state : anthropic-stream-state
;; 流状态记录。
;;
;; 返回值
;; ----
;; list
;; 已完成的 pi-tool-call-block 列表。

(import (liii check)
  (liii pi ai api anthropic-messages stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-anthropic-stream-state '() #f '() '() "end_turn" #f #f)))
  (check (anthropic-stream-state? state) => #t)
  (check (anthropic-stream-state-tool-calls state) => '())
) ;let*


(check-report)
