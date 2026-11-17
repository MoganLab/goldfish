;; anthropic-stream-state-usage
;; 读取 Anthropic 流状态中的用量统计。
;;
;; 语法
;; ----
;; (anthropic-stream-state-usage state)
;;
;; 参数
;; ----
;; state : anthropic-stream-state
;; 流状态记录。
;;
;; 返回值
;; ----
;; alist
;; 内部 usage alist，包含 input、output、cacheRead、cacheWrite 与 totalTokens。

(import (liii check)
  (liii pi ai api anthropic-messages stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-anthropic-stream-state '() #f '((input . 1) (output . 2) (cacheRead . 0) (cacheWrite . 0) (totalTokens . 3)) '() "end_turn" #f #f)))
  (check (anthropic-stream-state? state) => #t)
  (check (anthropic-stream-state-usage state) => '((input . 1) (output . 2) (cacheRead . 0) (cacheWrite . 0) (totalTokens . 3)))
) ;let*


(check-report)
