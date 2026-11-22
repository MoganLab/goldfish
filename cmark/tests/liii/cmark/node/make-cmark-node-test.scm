;; make-cmark-node
;; 创建一个通用的 cmark AST 节点。
;;
;; 语法
;; ----
;; (make-cmark-node type data children parent)
;;
;;
;; 参数
;; ----
;; type : any
;; 输入参数。
;; data : any
;; 输入参数。
;; children : any
;; 输入参数。
;; parent : any
;; 输入参数。
;; 返回值
;; ----
;; cmark-node
;; 返回携带 type、data、children 和 parent 的节点记录。

(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

(let ((node (make-cmark-node 'document '() '() #f)))
  (check-true (cmark-node? node))
  (check (cmark-node-type node) => 'document)
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
  (check-true (cmark-node-type? node 'document))
) ;let

;; 测试通用节点携带自定义数据
(let ((node (make-cmark-node 'custom '((key . "value")) (list (make-cmark-text "child")) #f)
      ) ;node
     ) ;
  (check (cmark-node-type node) => 'custom)
  (check (cdr (assoc 'key (cmark-node-data node))) => "value")
  (check (length (cmark-node-children node)) => 1)
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试通用节点带父节点
(let* ((p (make-cmark-node 'document '() '() #f))
       (child (make-cmark-node 'paragraph '() '() p))
      ) ;
  (check (cmark-node-parent child) => p)
) ;let*

(check-report)
