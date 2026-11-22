;; unquote-blockquote-line
;; 剥除引用行首的 > 标记及最多一个可选空白列。
;;
;; 语法
;; ----
;; (unquote-blockquote-line line)
;;
;; 参数
;; ----
;; line : string
;; 引用行字符串。
;;
;; 返回值
;; ----
;; string
;; 返回剥除前缀后的字符串。

(import (scheme base))
(import (liii check))
(import (liii cmark block blockquote))

(check-set-mode! 'report-failed)

(check (unquote-blockquote-line "> hello") => "hello")
(check (unquote-blockquote-line "plain") => "plain")

(check-report)
