;; %repl-string-common-prefix
;; 计算两个字符串的最长公共前缀。
;;
;; 语法
;; ----
;; (%repl-string-common-prefix a b)
;;
;; 参数
;; ----
;; a : string
;; 第一个字符串。
;; b : string
;; 第二个字符串。
;;
;; 返回值
;; ----
;; string
;; 两个字符串的最长公共前缀；没有公共字符时返回空字符串。

(import (scheme base) (liii check) (liii goldrepl complete))

(check-set-mode! 'report-failed)

;; 存在公共前缀
(check (%repl-string-common-prefix "string-append" "string-copy") => "string-")

;; 完全没有公共前缀
(check (%repl-string-common-prefix "apple" "banana") => "")

;; 相同字符串返回自身
(check (%repl-string-common-prefix "abc" "abc") => "abc")

;; 短串是长串的前缀
(check (%repl-string-common-prefix "ab" "abcdef") => "ab")

;; 空字符串
(check (%repl-string-common-prefix "" "abc") => "")

(check-report)
