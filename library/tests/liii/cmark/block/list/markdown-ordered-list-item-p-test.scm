;; markdown-ordered-list-item?
;; 判断行是否为有序列表项。
;;
;; 语法
;; ----
;; (markdown-ordered-list-item? line)
;;
;; 参数
;; ----
;; line : string
;; 输入行。
;;
;; 返回值
;; ----
;; boolean
;; 是有序列表项返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(check-true (markdown-ordered-list-item? "1) a"))

(check-report)
