;; make-cmark-thematic-break
;; 创建分隔线节点。
;;
;; 语法
;; ----
;; (make-cmark-thematic-break)
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

(let ((node (make-cmark-thematic-break)))
  (check (cmark-node-type node) => 'thematic_break)
  (check (cmark-node-data node) => '())
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

(check-report)
