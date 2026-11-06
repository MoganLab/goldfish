;; history-reset!
;; 重置历史浏览状态（清除 index 和 draft）。
;;
;; 语法
;; ----
;; (history-reset! h)
;;
;; 参数
;; ----
;; h : history?
;; 历史管理器实例。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui editor history))

(check-set-mode! 'report-failed)

(let ((h (make-history)))
  (history-push! h "cmd")
  (history-prev h "draft")
  (check (history-index h) => 0)
  (history-reset! h)
  (check-false (history-index h))
  (check-false (history-draft h))
) ;let

(check-report)
