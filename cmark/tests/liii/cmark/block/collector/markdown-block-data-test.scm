;; markdown-block-data
;; 获取 markdown-block 的附加数据 alist。
;;
;; 语法
;; ----
;; (markdown-block-data block)
;;
;; 参数
;; ----
;; block : markdown-block
;; 块记录。
;;
;; 返回值
;; ----
;; alist
;; 返回 data alist。

(import (scheme base))
(import (liii check))
(import (liii cmark block collector))

(check-set-mode! 'report-failed)

(check
  (markdown-block-data (make-markdown-block 'heading '() '((level . 2))))
  =>
  '((level . 2))
) ;check

(check-report)
