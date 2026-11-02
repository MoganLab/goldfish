;; anthropic-stream-state-usage-set!
;; 设置 Anthropic 流状态中的用量统计。
;;
;; 语法
;; ----
;; (anthropic-stream-state-usage-set! state value)
;;
;; 参数
;; ----
;; state : anthropic-stream-state | vector
;; 流状态；record 为当前表示，vector 用于兼容旧调用。
;; value : alist
;; 内部 usage alist，包含 input、output、cacheRead、cacheWrite 与 totalTokens。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新流状态。

;; 说明
;; ----
;; record 状态直接更新记录字段；vector 状态更新对应的下标。

(import (liii check)
  (liii pi ai api anthropic-messages stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-anthropic-stream-state '() #f '() '() "end_turn" #f #f)))
  (anthropic-stream-state-usage-set! state '((input . 4) (output . 5) (cacheRead . 0) (cacheWrite . 0) (totalTokens . 9)))
  (check (anthropic-stream-state-usage state) => '((input . 4) (output . 5) (cacheRead . 0) (cacheWrite . 0) (totalTokens . 9)))
) ;let*

;; vector 状态（兼容旧调用）
(let ((state (vector #f #f '() '() "end_turn" #f #f)))
  (anthropic-stream-state-usage-set! state '((input . 4) (output . 5) (cacheRead . 0) (cacheWrite . 0) (totalTokens . 9)))
  (check (anthropic-stream-state-usage state) => '((input . 4) (output . 5) (cacheRead . 0) (cacheWrite . 0) (totalTokens . 9)))
) ;let*

(check-report)
