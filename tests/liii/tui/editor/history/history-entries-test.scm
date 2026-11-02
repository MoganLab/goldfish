;; history-entries
;; 获取历史管理器中保存的全部条目列表（最新条目排在最前）。
;;
;; 语法
;; ----
;; (history-entries h)
;;
;; 参数
;; ----
;; h : history?
;; 历史管理器实例。
;;
;; 返回值
;; ----
;; list?
;; 历史文本列表。

(import (scheme base) (liii check) (liii tui editor history))

(check-set-mode! 'report-failed)

(let ((h (make-history)))
  (check (history-entries h) => '())
  (history-push! h "first")
  (history-push! h "second")
  (check (history-entries h) => '("second" "first"))
) ;let

(check-report)
