(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

;; make-cmark-document
;; 创建文档根节点。
;;
;; 语法
;; ----
;; (make-cmark-document)
;;
;; 返回值
;; ----
;; cmark-node
;; 返回类型为 'document 的根节点。

(let ((node (make-cmark-document)))
  (check (cmark-node-type node) => 'document)
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
  (check-true (cmark-document? node))
) ;let

;; 测试文档作为根节点包含子节点
(let ((doc (make-cmark-document))
      (heading (make-cmark-heading 1))
      (para (make-cmark-paragraph))
     ) ;
  (cmark-node-append-child! doc heading)
  (cmark-node-append-child! doc para)
  (check (length (cmark-node-children doc)) => 2)
  (check (cmark-node-type (cmark-node-first-child doc)) => 'heading)
  (check (cmark-node-type (cmark-node-last-child doc)) => 'paragraph)
  (check (cmark-node-parent heading) => doc)
  (check (cmark-node-parent para) => doc)
) ;let

(check-report)
