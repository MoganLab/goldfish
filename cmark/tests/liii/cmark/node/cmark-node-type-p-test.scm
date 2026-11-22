;; cmark-node-type?
;; 判断 cmark-node 是否属于指定节点类型。
;;
;; 语法
;; ----
;; (cmark-node-type? node type)
;;
;; 参数
;; ----
;; node : any
;; 待检查对象。
;; type : symbol
;; 期望节点类型。
;;
;; 返回值
;; ----
;; boolean
;; 是 cmark-node 且类型匹配返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (cmark-node-type? (make-cmark-paragraph) 'paragraph))
(check-false (cmark-node-type? (make-cmark-paragraph) 'heading))
(check-false (cmark-node-type? (make-cmark-text "x") 'paragraph))
(check-false (cmark-node-type? "not-node" 'paragraph))
(check-false (cmark-node-type? #f 'paragraph))

(check-report)
