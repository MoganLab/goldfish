;; cmark-node-data
;; 获取 cmark-node 的附加数据 alist。
;;
;; 语法
;; ----
;; (cmark-node-data node)
;;
;; 参数
;; ----
;; node : cmark-node
;; AST 节点。
;;
;; 返回值
;; ----
;; alist
;; 返回节点附加数据 alist。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check (cmark-node-data (make-cmark-paragraph)) => '())
(check (cmark-node-data (make-cmark-heading 3)) => '((level . 3)))

(check-report)
