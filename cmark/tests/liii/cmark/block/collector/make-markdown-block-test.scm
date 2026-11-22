;; make-markdown-block
;; 创建 markdown-block 记录实例。
;;
;; 语法
;; ----
;; (make-markdown-block type lines data)
;;
;; 参数
;; ----
;; type : symbol
;; 块类型。
;; lines : list
;; 行列表。
;; data : alist
;; 附加数据。
;;
;; 返回值
;; ----
;; markdown-block
;; 返回新创建的记录实例。

(import (scheme base))
(import (liii check))
(import (liii cmark block collector))

(check-set-mode! 'report-failed)

(let ((b (make-markdown-block 'paragraph '("hi") '())))
  (check-true (markdown-block? b))
) ;let

(check-report)
