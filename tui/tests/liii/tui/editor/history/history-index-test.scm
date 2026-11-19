;; history-index
;; 获取当前浏览的历史索引（#f 表示未处于历史浏览状态）。
;;
;; 语法
;; ----
;; (history-index h)
;;
;; 参数
;; ----
;; h : history?
;; 历史管理器实例。
;;
;; 返回值
;; ----
;; integer? 或 #f
;; 0-based 历史索引，或 #f。

(import (scheme base) (liii check) (liii tui editor history))

(check-set-mode! 'report-failed)

(let ((h (make-history)))
  (check-false (history-index h))
  (history-push! h "cmd1")
  (history-prev h "draft")
  (check (history-index h) => 0)
) ;let

(check-report)
