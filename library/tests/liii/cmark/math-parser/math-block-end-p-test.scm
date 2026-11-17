
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark inline math))

(check-set-mode! 'report-failed)

;; math-block-end?
;; 判断一行文本是否是块级数学公式的结束标记。
;;
;; 语法
;; ----
;; (math-block-end? line)
;;
;; 返回值
;; ----
;; boolean
;; 当 line 为 "$$" 或 "\\]" 时返回 #t。

(check-true (math-block-end? "$$"))
(check-false (math-block-end? "$$ "))
(check-true (math-block-end? "\\]"))
(check-false (math-block-end? "\\] "))

(check-report)
