;; blank-line?
;; 判断字符串是否为空行（严格空串）。
;;
;; 语法
;; ----
;; (blank-line? line)
;;
;; 参数
;; ----
;; line : string
;; 输入行字符串。
;;
;; 返回值
;; ----
;; boolean
;; 为空串返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block line))

(check-set-mode! 'report-failed)

(check-true (blank-line? ""))
(check-false (blank-line? " "))

(check-report)
