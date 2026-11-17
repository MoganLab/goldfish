;; list-item-marker-line?
;; 判断行是否为列表项标记行（有序或无序）。
;;
;; 语法
;; ----
;; (list-item-marker-line? line)
;;
;; 参数
;; ----
;; line : string
;; 输入行。
;;
;; 返回值
;; ----
;; boolean
;; 是列表项标记行返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(check-true (list-item-marker-line? "- a"))
(check-true (list-item-marker-line? "1. a"))
(check-false (list-item-marker-line? "plain"))

(check-report)
