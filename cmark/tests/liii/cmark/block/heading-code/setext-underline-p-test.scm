;; setext-underline?
;; 判断行是否为 Setext 标题下划线（= 或 -）。
;;
;; 语法
;; ----
;; (setext-underline? line)
;;
;; 参数
;; ----
;; line : string
;; 输入行。
;;
;; 返回值
;; ----
;; boolean
;; 是下划线返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block heading-code))

(check-set-mode! 'report-failed)

(check-true (setext-underline? "==="))
(check-true (setext-underline? "-"))
(check-false (setext-underline? "= -"))

(check-report)
