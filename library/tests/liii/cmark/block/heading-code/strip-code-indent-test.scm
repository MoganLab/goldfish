;; strip-code-indent
;; 剥除代码行至多 indent 个前导空格。
;;
;; 语法
;; ----
;; (strip-code-indent indent line)
;;
;; 参数
;; ----
;; indent : integer
;; 最大剥除空格数。
;; line : string
;; 输入行。
;;
;; 返回值
;; ----
;; string
;; 返回去缩进后的字符串。

(import (scheme base))
(import (liii check))
(import (liii cmark block heading-code))

(check-set-mode! 'report-failed)

(check (strip-code-indent 2 "   code") => " code")

(check-report)
