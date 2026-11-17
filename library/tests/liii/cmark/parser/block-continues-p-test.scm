(import (liii check) (liii cmark block parser))

(check-set-mode! 'report-failed)

;; block-continues?
;; 判断某个块级节点在给定新行下是否继续延伸。
;;
;; 语法
;; ----
;; (block-continues? block-type line)

(check-true (block-continues? 'paragraph "More text"))
(check-false (block-continues? 'paragraph ""))
(check-false (block-continues? 'paragraph "# Heading"))

(check-report)
