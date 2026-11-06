;; history-next
;; 向下回退较新的历史记录，回退到最新之外时恢复暂存的草稿。
;;
;; 语法
;; ----
;; (history-next h current-text)
;;
;; 参数
;; ----
;; h : history?
;; 历史管理器实例。
;;
;; current-text : string?
;; 当前文本内容。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 较新的历史记录或草稿文本；不在浏览状态时返回 #f。

(import (scheme base) (liii check) (liii tui editor history))

(check-set-mode! 'report-failed)

(let ((h (make-history)))
  (history-push! h "one")
  (history-push! h "two")
  (history-prev h "draft")
  (history-prev h "two")
  ;; 从 "one" 向下回退
  (check (history-next h "one") => "two")
  ;; 再次向下恢复草稿
  (check (history-next h "two") => "draft")
  (check-false (history-index h))
) ;let

(check-report)
