;; string-starts-with?
;; 判断字符串是否以指定的前缀子串开头。
;;
;; 语法
;; ----
;; (string-starts-with? str prefix)
;;
;; 参数
;; ----
;; str : string      母串
;; prefix : string   前缀子串
;;
;; 返回值
;; ------
;; boolean
;; 母串以该前缀开头返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii goldcheck path))

(check-set-mode! 'report-failed)

;; 1. 以前缀开头返回 #t
(check (string-starts-with? "goldfish/liii/base.scm" "goldfish/") => #t)
(check (string-starts-with? "hello" "hello") => #t)
(check (string-starts-with? "hello" "") => #t)

;; 2. 不以前缀开头返回 #f
(check (string-starts-with? "goldfish/liii/base.scm" "tools/") => #f)
(check (string-starts-with? "hi" "hello") => #f)

(check-report)
