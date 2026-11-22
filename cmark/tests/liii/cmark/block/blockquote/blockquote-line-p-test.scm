;; blockquote-line?
;; 判断行 line 是否以引用标记 > 开头（前导空格不超过 3）。
;;
;; 语法
;; ----
;; (blockquote-line? line)
;;
;; 参数
;; ----
;; line : string
;; 输入行字符串。
;;
;; 返回值
;; ----
;; boolean
;; 是引用行返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block blockquote))

(check-set-mode! 'report-failed)

(check-true (blockquote-line? "> quote"))
(check-false (blockquote-line? "    > code"))

(check-report)
