;; task-list-item-status
;; 识别任务列表项勾选状态（checked / unchecked / #f）。
;;
;; 语法
;; ----
;; (task-list-item-status line)
;;
;; 参数
;; ----
;; line : string
;; 列表项内容行。
;;
;; 返回值
;; ----
;; symbol 或 #f
;; 返回 checked、unchecked 或 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(check (task-list-item-status "[x] done") => 'checked)
(check (task-list-item-status "[ ] todo") => 'unchecked)
(check (task-list-item-status "plain") => #f)

(check-report)
