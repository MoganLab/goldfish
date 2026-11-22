;; strip-trailing-carriage-return
;; 去除字符串末尾的回车符 CR。
;;
;; 语法
;; ----
;; (strip-trailing-carriage-return str)
;;
;; 参数
;; ----
;; str : string
;; 目标字符串。
;;
;; 返回值
;; ----
;; string
;; 去除末尾 #\\return 后的字符串。

(import (scheme base))
(import (liii check))
(import (liii cmark text))

(check-set-mode! 'report-failed)

(check (strip-trailing-carriage-return "abc\r") => "abc")
(check (strip-trailing-carriage-return "abc") => "abc")

(check-report)
