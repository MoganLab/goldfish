;; anthropic-stream-state-current-tool
;; 读取 Anthropic 流状态中当前正在进行的工具块状态。
;;
;; 语法
;; ----
;; (anthropic-stream-state-current-tool state)
;;
;; 参数
;; ----
;; state : anthropic-stream-state | vector
;; 流状态；record 为当前表示，vector 用于兼容旧调用。
;;
;; 返回值
;; ----
;; any
;; 当前工具块状态；没有活动工具块时为 #f。

(import (liii check)
  (liii pi ai api anthropic-messages stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-anthropic-stream-state '() #f '() '() "end_turn" #f #f)))
  (check (anthropic-stream-state? state) => #t)
  (check (anthropic-stream-state-current-tool state) => #f)
) ;let*

;; vector 状态（兼容旧调用）
(let ((state (vector '() #f '() '() "end_turn" #f #f)))
  (check (anthropic-stream-state-current-tool state) => #f)
) ;let*

(check-report)
