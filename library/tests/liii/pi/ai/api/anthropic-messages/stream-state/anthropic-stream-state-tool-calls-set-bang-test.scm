;; anthropic-stream-state-tool-calls-set!
;; 设置 Anthropic 流状态中已完成的工具调用列表。
;;
;; 语法
;; ----
;; (anthropic-stream-state-tool-calls-set! state value)
;;
;; 参数
;; ----
;; state : anthropic-stream-state
;; 流状态记录。
;; value : list
;; 已完成的 pi-tool-call-block 列表。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新流状态。


(import (liii check)
  (liii pi ai api anthropic-messages stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-anthropic-stream-state '() #f '() '() "end_turn" #f #f)))
  (anthropic-stream-state-tool-calls-set! state '())
  (check (anthropic-stream-state-tool-calls state) => '())
) ;let*


(check-report)
