;; string-prefix-at?
;; 判断字符串 str 从 pos 位置起是否以 prefix 开头。
;;
;; 语法
;; ----
;; (string-prefix-at? str pos prefix)
;;
;; 参数
;; ----
;; str : string
;; 目标字符串。
;; pos : integer
;; 起始位置。
;; prefix : string
;; 前缀字符串。
;;
;; 返回值
;; ----
;; boolean
;; 匹配返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark text))

(check-set-mode! 'report-failed)

(check-true (string-prefix-at? "abcdef" 2 "cd"))
(check-false (string-prefix-at? "abcdef" 5 "ef"))

(check-report)
