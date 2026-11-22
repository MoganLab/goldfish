;; thematic-break-line?
;; 判断行是否为主题分隔线（--- / *** / ___）。
;;
;; 语法
;; ----
;; (thematic-break-line? line)
;;
;; 参数
;; ----
;; line : string
;; 输入行字符串。
;;
;; 返回值
;; ----
;; boolean
;; 是分隔线返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block line))

(check-set-mode! 'report-failed)

(check-true (thematic-break-line? "---"))
(check-true (thematic-break-line? " * * * "))
(check-false (thematic-break-line? "--"))

(check-report)
