;; history-prev
;; 向上回溯更早的历史记录，首次回溯暂存当前草稿。
;;
;; 语法
;; ----
;; (history-prev h current-text)
;;
;; 参数
;; ----
;; h : history?
;; 历史管理器实例。
;;
;; current-text : string?
;; 当前编辑器未提交的文本。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 回溯得到的历史记录；历史为空时返回 #f。

(import (scheme base) (liii check) (liii tui editor history))

(check-set-mode! 'report-failed)

(let ((h (make-history)))
  (history-push! h "one")
  (history-push! h "two")
  (check (history-prev h "draft") => "two")
  (check (history-prev h "two") => "one")
  ;; 到底停在最旧一条
  (check (history-prev h "one") => "one")
) ;let

(check-report)
