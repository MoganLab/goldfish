;; all-spaces?
;; 判断字符串是否全部由空格字符组成。
;;
;; 语法
;; ----
;; (all-spaces? str)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;;
;; 返回值
;; ----
;; boolean
;; 全为空格返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline char))

(check-set-mode! 'report-failed)

(check-true (all-spaces? "   "))
(check-false (all-spaces? " a "))

(check-report)
