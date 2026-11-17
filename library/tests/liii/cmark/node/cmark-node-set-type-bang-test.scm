;; cmark-node-set-type!
;; 修改 cmark-node 的节点类型符号。
;;
;; 语法
;; ----
;; (cmark-node-set-type! node type)
;;
;; 参数
;; ----
;; node : cmark-node
;; AST 节点。
;; type : symbol
;; 新的节点类型符号。
;;
;; 返回值
;; ----
;; unspecified
;; 原地更新节点类型。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(let ((n (make-cmark-paragraph)))
  (cmark-node-set-type! n 'heading)
  (check (cmark-node-type n) => 'heading)
) ;let

(check-report)
