;; cmark-node-last-child
;; 获取 cmark-node 的最后一个子节点。
;;
;; 语法
;; ----
;; (cmark-node-last-child node)
;;
;; 参数
;; ----
;; node : cmark-node
;; AST 节点。
;;
;; 返回值
;; ----
;; cmark-node 或 #f
;; 存在子节点时返回最后一个子节点，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(let ((p (make-cmark-paragraph))
      (t1 (make-cmark-text "1"))
      (t2 (make-cmark-text "2"))
     ) ;
  (check (cmark-node-last-child p) => #f)
  (cmark-node-append-child! p t1)
  (cmark-node-append-child! p t2)
  (check (cmark-node-last-child p) => t2)
) ;let

(check-report)
