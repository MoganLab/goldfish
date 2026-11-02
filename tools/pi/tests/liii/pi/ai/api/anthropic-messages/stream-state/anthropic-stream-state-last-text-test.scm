;; anthropic-stream-state-last-text
;; 读取 Anthropic 流状态中当前正在累积的文本。
;;
;; 语法
;; ----
;; (anthropic-stream-state-last-text state)
;;
;; 参数
;; ----
;; state : anthropic-stream-state | vector
;; 流状态；record 为当前表示，vector 用于兼容旧调用。
;;
;; 返回值
;; ----
;; string | #f
;; 当前累积的文本；没有活动文本块时为 #f。

(import (liii check)
  (liii pi ai api anthropic-messages stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-anthropic-stream-state '() "partial" '() '() "end_turn" #f #f)))
  (check (anthropic-stream-state? state) => #t)
  (check (anthropic-stream-state-last-text state) => "partial")
) ;let*

;; vector 状态（兼容旧调用）
(let ((state (vector '() "partial" '() '() "end_turn" #f #f)))
  (check (anthropic-stream-state-last-text state) => "partial")
) ;let*

(check-report)
