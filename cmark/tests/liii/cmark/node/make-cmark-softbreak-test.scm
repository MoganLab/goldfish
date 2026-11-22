;; make-cmark-softbreak
;; 创建软换行节点。
;;
;; 语法
;; ----
;; (make-cmark-softbreak)
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

(let ((node (make-cmark-softbreak)))
  (check (cmark-node-type node) => 'softbreak)
  (check (cmark-node-data node) => '())
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

(check-report)
