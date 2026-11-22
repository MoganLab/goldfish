;; make-cmark-linebreak
;; 创建硬换行节点。
;;
;; 语法
;; ----
;; (make-cmark-linebreak)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ----
;; cmark-node
;; 返回新创建的 cmark AST 节点。

(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

(let ((node (make-cmark-linebreak)))
  (check (cmark-node-type node) => 'linebreak)
  (check (cmark-node-data node) => '())
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

(check-report)
