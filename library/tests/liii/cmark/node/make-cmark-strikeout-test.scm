;; make-cmark-strikeout
;; 创建删除线节点。
;;
;; 语法
;; ----
;; (make-cmark-strikeout)
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

(let ((node (make-cmark-strikeout)))
  (check (cmark-node-type node) => 'strikeout)
  (check (cmark-node-data node) => '())
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试作为容器追加子节点
(let ((node (make-cmark-strikeout)))
  (let ((child (make-cmark-text "deleted")))
    (cmark-node-append-child! node child)
    (check (length (cmark-node-children node)) => 1)
    (check (cmark-node-type (cmark-node-first-child node)) => 'text)
    (check (cmark-node-parent child) => node)
  ) ;let
) ;let

(check-report)
