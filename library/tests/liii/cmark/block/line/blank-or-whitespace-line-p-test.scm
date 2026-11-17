;; blank-or-whitespace-line?
;; 判断行是否仅包含空白字符。
;;
;; 语法
;; ----
;; (blank-or-whitespace-line? line)
;;
;; 参数
;; ----
;; line : string
;; 输入行字符串。
;;
;; 返回值
;; ----
;; boolean
;; 全空白返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block line))

(check-set-mode! 'report-failed)

(check-true (blank-or-whitespace-line? "  \t "))
(check-false (blank-or-whitespace-line? " a "))

(check-report)
