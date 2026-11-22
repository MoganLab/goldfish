;; count-leading-indent
;; 统计字符串前导空格和 tab 的总字符数。
;;
;; 语法
;; ----
;; (count-leading-indent line)
;;
;; 参数
;; ----
;; line : string
;; 输入行字符串。
;;
;; 返回值
;; ----
;; integer
;; 返回前导缩进字符数。

(import (scheme base))
(import (liii check))
(import (liii cmark text))

(check-set-mode! 'report-failed)

(check (count-leading-indent " \t x") => 3)
(check (count-leading-indent "abc") => 0)

(check-report)
