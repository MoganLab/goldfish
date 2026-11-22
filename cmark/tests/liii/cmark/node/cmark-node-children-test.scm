;; cmark-node-children
;; 获取 cmark-node 的子节点列表。
;;
;; 语法
;; ----
;; (cmark-node-children node)
;;
;; 参数
;; ----
;; node : cmark-node
;; AST 节点。
;;
;; 返回值
;; ----
;; list
;; 返回子节点列表。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(let ((p (make-cmark-paragraph)) (t (make-cmark-text "a")))
  (check (cmark-node-children p) => '())
  (cmark-node-append-child! p t)
  (check (cmark-node-children p) => (list t))
) ;let

(check-report)
