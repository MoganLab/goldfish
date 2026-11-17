;; fenced-code-start?
;; 判断行是否为围栏代码块起始行。
;;
;; 语法
;; ----
;; (fenced-code-start? line)
;;
;; 参数
;; ----
;; line : string
;; 输入行。
;;
;; 返回值
;; ----
;; boolean
;; 是围栏起始返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block heading-code))

(check-set-mode! 'report-failed)

(check-true (fenced-code-start? "```"))
(check-false (fenced-code-start? "``"))

(check-report)
