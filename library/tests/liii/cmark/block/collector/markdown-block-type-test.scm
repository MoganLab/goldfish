;; markdown-block-type
;; 获取 markdown-block 的类型符号。
;;
;; 语法
;; ----
;; (markdown-block-type block)
;;
;; 参数
;; ----
;; block : markdown-block
;; 块记录。
;;
;; 返回值
;; ----
;; symbol
;; 返回类型符号。

(import (scheme base))
(import (liii check))
(import (liii cmark block collector))

(check-set-mode! 'report-failed)

(check (markdown-block-type (make-markdown-block 'heading '() '())) => 'heading)

(check-report)
