;; math-block-start-line?
;; 判断行 line 是否为块级数学公式起始行。
;;
;; 语法
;; ----
;; (math-block-start-line? line)
;;
;; 参数
;; ----
;; line : string
;; 输入行字符串。
;;
;; 返回值
;; ----
;; boolean
;; 是数学块起始返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block math))

(check-set-mode! 'report-failed)

(check-true (math-block-start-line? "$$"))
(check-true (math-block-start-line? "\\[x\\]"))
(check-false (math-block-start-line? "hello"))

(check-report)
