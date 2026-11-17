;; markdown-list-item-indent
;; 获取列表项标记前导空格数。
;;
;; 语法
;; ----
;; (markdown-list-item-indent line)
;;
;; 参数
;; ----
;; line : string
;; 列表项行。
;;
;; 返回值
;; ----
;; integer
;; 返回前导空格数。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(check (markdown-list-item-indent "  - a") => 2)

(check-report)
