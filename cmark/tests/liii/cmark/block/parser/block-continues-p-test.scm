;; block-continues?
;; 判断某个块级节点在给定新行下是否继续延伸。
;;
;; 语法
;; ----
;; (block-continues? block-type line)
;;
;; 参数
;; ----
;; block-type : any
;; 输入参数。
;; line : any
;; 输入参数。
;;
;; 返回值
;; ----
;; boolean
;; 满足条件返回 #t，否则返回 #f。

(import (liii check) (liii cmark block parser))

(check-set-mode! 'report-failed)

(check-true (block-continues? 'paragraph "More text"))
(check-false (block-continues? 'paragraph ""))
(check-false (block-continues? 'paragraph "# Heading"))

(check-report)
