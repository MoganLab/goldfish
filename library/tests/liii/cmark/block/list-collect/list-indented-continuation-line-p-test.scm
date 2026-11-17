;; list-indented-continuation-line?
;; 判断行是否为满足最小缩进要求的列表续行。
;;
;; 语法
;; ----
;; (list-indented-continuation-line? line . maybe-indent)
;;
;; 参数
;; ----
;; line : string
;; 输入行。
;;
;; 返回值
;; ----
;; boolean
;; 是续行返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block list-collect))

(check-set-mode! 'report-failed)

(check-true (list-indented-continuation-line? "  cont" 2))
(check-false (list-indented-continuation-line? "cont" 2))

(check-report)
