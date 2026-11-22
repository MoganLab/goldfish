;; cmark-node-type
;; 获取 cmark-node 的节点类型符号。
;;
;; 语法
;; ----
;; (cmark-node-type node)
;;
;; 参数
;; ----
;; node : cmark-node
;; AST 节点。
;;
;; 返回值
;; ----
;; symbol
;; 返回节点类型符号。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check (cmark-node-type (make-cmark-paragraph)) => 'paragraph)
(check (cmark-node-type (make-cmark-heading 2)) => 'heading)

(check-report)
