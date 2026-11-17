(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

;; make-cmark-linebreak
;; 创建硬换行节点。
;;
;; 语法
;; ----
;; (make-cmark-linebreak)

(let ((node (make-cmark-linebreak)))
  (check (cmark-node-type node) => 'linebreak)
  (check (cmark-node-data node) => '())
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

(check-report)
