;; indented-code-line?
;; 判断行是否为 4 空格或 1 tab 缩进的代码行。
;;
;; 语法
;; ----
;; (indented-code-line? line)
;;
;; 参数
;; ----
;; line : string
;; 输入行。
;;
;; 返回值
;; ----
;; boolean
;; 是缩进代码行返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block heading-code))

(check-set-mode! 'report-failed)

(check-true (indented-code-line? "    x"))
(check-false (indented-code-line? "   x"))

(check-report)
