;; expand-leading-tabs
;; 按 4 列 tab stop 展开行首前导制表符。
;;
;; 语法
;; ----
;; (expand-leading-tabs line . maybe-start-col)
;;
;; 参数
;; ----
;; line : string
;; 输入行字符串。
;;
;; 返回值
;; ----
;; string
;; 返回展开前导 tab 后的字符串。

(import (scheme base))
(import (liii check))
(import (liii cmark block line))

(check-set-mode! 'report-failed)

(check (expand-leading-tabs "\tfoo") => "    foo")
(check (expand-leading-tabs "  \tfoo") => "    foo")

(check-report)
