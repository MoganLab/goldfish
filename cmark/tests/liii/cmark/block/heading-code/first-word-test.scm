;; first-word
;; 提取字符串中的第一个空白分隔单词。
;;
;; 语法
;; ----
;; (first-word str)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;;
;; 返回值
;; ----
;; string
;; 返回第一个单词。

(import (scheme base))
(import (liii check))
(import (liii cmark block heading-code))

(check-set-mode! 'report-failed)

(check (first-word "scheme hello") => "scheme")
(check (first-word "") => "")

(check-report)
