;; anthropic-stream-state-current-tool-set!
;; 设置 Anthropic 流状态中当前正在进行的工具块状态。
;;
;; 语法
;; ----
;; (anthropic-stream-state-current-tool-set! state value)
;;
;; 参数
;; ----
;; state : anthropic-stream-state
;; 流状态记录。
;; value : any
;; 当前工具块状态；没有活动工具块时为 #f。
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
  (anthropic-stream-state-current-tool-set! state #f)
  (check (anthropic-stream-state-current-tool state) => #f)
) ;let*


(check-report)
