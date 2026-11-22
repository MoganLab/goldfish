;; cmark-node-parent
;; 获取 cmark-node 的父节点。
;;
;; 语法
;; ----
;; (cmark-node-parent node)
;;
;; 参数
;; ----
;; node : cmark-node
;; AST 节点。
;;
;; 返回值
;; ----
;; cmark-node 或 #f
;; 有父节点时返回父节点，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(let ((p (make-cmark-paragraph)) (t (make-cmark-text "a")))
  (check (cmark-node-parent t) => #f)
  (cmark-node-append-child! p t)
  (check (cmark-node-parent t) => p)
) ;let

(check-report)
