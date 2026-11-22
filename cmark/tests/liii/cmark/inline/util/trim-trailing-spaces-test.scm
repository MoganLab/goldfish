;; trim-trailing-spaces
;; 去除字符串末尾的空格字符。
;;
;; 语法
;; ----
;; (trim-trailing-spaces str)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;;
;; 返回值
;; ----
;; string
;; 去除尾部空格后的字符串。

(import (scheme base))
(import (liii check))
(import (liii cmark inline util))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check (trim-trailing-spaces "abc   ") => "abc")

(check-report)
