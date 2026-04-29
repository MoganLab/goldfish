(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; string-append
;; 将多个字符串连接为一个新字符串。
;;
;; 语法
;; ----
;; (string-append string ...)
;;
;; 参数
;; ----
;; string ... : string?
;; 任意数量的字符串。
;;
;; 返回值
;; ------
;; string?
;; 所有输入字符串按顺序连接后的新字符串。
;;
;; 说明
;; ----
;; 1. 无参数时返回空字符串
;; 2. 单参数时返回该参数本身
;; 3. 不修改原字符串，返回新字符串
(check (string-append) => "")
(check (string-append "abc") => "abc")
(check (string-append "abc" "def") => "abcdef")
(check (string-append "a" "b" "c") => "abc")
(check (string-append "" "abc") => "abc")
(check (string-append "abc" "") => "abc")
(check (string-append "" "") => "")
(check (string-append "Hello, " "world!") => "Hello, world!")
(check-catch 'wrong-type-arg (string-append 'a))
(check-catch 'wrong-type-arg (string-append "a" 1))
(check-catch 'wrong-type-arg (string-append "a" '(1)))
(check-report)
