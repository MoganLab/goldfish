;; strip-task-list-item-marker
;; 剥除任务列表项前缀的 [ ] / [x] 复选框。
;;
;; 语法
;; ----
;; (strip-task-list-item-marker line)
;;
;; 参数
;; ----
;; line : string
;; 列表项内容行。
;;
;; 返回值
;; ----
;; string
;; 返回去除复选框后的文本。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(check (strip-task-list-item-marker "[x] done") => "done")

(check-report)
