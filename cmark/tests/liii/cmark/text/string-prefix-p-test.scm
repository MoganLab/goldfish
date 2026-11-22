;; string-prefix?
;; 判断字符串 str 是否以 prefix 开头。
;;
;; 语法
;; ----
;; (string-prefix? prefix str)
;;
;; 参数
;; ----
;; prefix : string
;; 前缀字符串。
;; str : string
;; 目标字符串。
;;
;; 返回值
;; ----
;; boolean
;; 以 prefix 开头返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark text))

(check-set-mode! 'report-failed)

(check-true (string-prefix? "he" "hello"))
(check-false (string-prefix? "world" "hello"))

(check-report)
