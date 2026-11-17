;; markdown-block-lines
;; 获取 markdown-block 的行列表。
;;
;; 语法
;; ----
;; (markdown-block-lines block)
;;
;; 参数
;; ----
;; block : markdown-block
;; 块记录。
;;
;; 返回值
;; ----
;; list
;; 返回行列表。

(import (scheme base))
(import (liii check))
(import (liii cmark block collector))

(check-set-mode! 'report-failed)

(check (markdown-block-lines (make-markdown-block 'paragraph '("a") '()))
  =>
  '("a")
) ;check

(check-report)
