;; anthropic-stream-state-last-text-set!
;; 设置 Anthropic 流状态中当前正在累积的文本。
;;
;; 语法
;; ----
;; (anthropic-stream-state-last-text-set! state value)
;;
;; 参数
;; ----
;; state : anthropic-stream-state
;; 流状态记录。
;; value : string | #f
;; 当前累积的文本；没有活动文本块时为 #f。
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
  (anthropic-stream-state-last-text-set! state "partial")
  (check (anthropic-stream-state-last-text state) => "partial")
) ;let*


(check-report)
