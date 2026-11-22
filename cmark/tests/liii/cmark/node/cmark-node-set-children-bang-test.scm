;; cmark-node-set-children!
;; 设置 cmark-node 的子节点列表。
;;
;; 语法
;; ----
;; (cmark-node-set-children! node children)
;;
;; 参数
;; ----
;; node : cmark-node
;; AST 节点。
;; children : list
;; 新的子节点列表。
;;
;; 返回值
;; ----
;; unspecified
;; 原地更新子节点列表。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(let ((p (make-cmark-paragraph)) (t (make-cmark-text "b")))
  (cmark-node-set-children! p (list t))
  (check (cmark-node-children p) => (list t))
) ;let

(check-report)
