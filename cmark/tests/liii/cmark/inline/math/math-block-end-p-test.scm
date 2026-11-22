;; math-block-end?
;; 判断一行文本是否是块级数学公式的结束标记。
;;
;; 语法
;; ----
;; (math-block-end? line)
;;
;;
;; 参数
;; ----
;; line : any
;; 输入参数。
;; 返回值
;; ----
;; boolean
;; 当 line 为 "$$" 或 "\\]" 时返回 #t。

(import (liii check) (liii cmark inline math))

(check-set-mode! 'report-failed)

(check-true (math-block-end? "$$"))
(check-false (math-block-end? "$$ "))
(check-true (math-block-end? "\\]"))
(check-false (math-block-end? "\\] "))

(check-report)
