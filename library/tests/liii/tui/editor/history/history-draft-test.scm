;; history-draft
;; 获取开始回溯历史前保存的未提交输入草稿。
;;
;; 语法
;; ----
;; (history-draft h)
;;
;; 参数
;; ----
;; h : history?
;; 历史管理器实例。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 暂存的草稿文本，或 #f。

(import (scheme base) (liii check) (liii tui editor history))

(check-set-mode! 'report-failed)

(let ((h (make-history)))
  (check-false (history-draft h))
  (history-push! h "prev-cmd")
  (history-prev h "my-draft")
  (check (history-draft h) => "my-draft")
) ;let

(check-report)
