;; html-block-start?
;; 判断行 line 是否为块级 HTML 起始。
;;
;; 语法
;; ----
;; (html-block-start? line)
;;
;; 参数
;; ----
;; line : string
;; 输入行字符串。
;;
;; 返回值
;; ----
;; boolean
;; 是 HTML 块起始返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block html))

(check-set-mode! 'report-failed)

(check-true (html-block-start? "<div>"))
(check-false (html-block-start? "plain"))

(check-report)
