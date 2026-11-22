;; empty-list-item?
;; 判断列表项标记行内容是否为空。
;;
;; 语法
;; ----
;; (empty-list-item? line)
;;
;; 参数
;; ----
;; line : string
;; 列表项标记行。
;;
;; 返回值
;; ----
;; boolean
;; 内容为空返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(check-true (empty-list-item? "-"))
(check-false (empty-list-item? "- a"))

(check-report)
