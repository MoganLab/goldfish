(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

;; make-cmark-block-quote
;; 创建块引用节点。
;;
;; 语法
;; ----
;; (make-cmark-block-quote)

(let ((node (make-cmark-block-quote)))
  (check (cmark-node-type node) => 'block_quote)
  (check (cmark-node-data node) => '())
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试作为容器追加子节点
(let ((bq (make-cmark-block-quote)))
  (let ((para (make-cmark-paragraph)) (heading (make-cmark-heading 2)))
    (cmark-node-append-child! bq para)
    (cmark-node-append-child! bq heading)
    (check (length (cmark-node-children bq)) => 2)
    (check (cmark-node-type (cmark-node-first-child bq)) => 'paragraph)
    (check (cmark-node-type (cmark-node-last-child bq)) => 'heading)
    (check (cmark-node-parent para) => bq)
    (check (cmark-node-parent heading) => bq)
  ) ;let
) ;let

;; 测试嵌套 blockquote
(let ((outer (make-cmark-block-quote)) (inner (make-cmark-block-quote)))
  (cmark-node-append-child! outer inner)
  (check (length (cmark-node-children outer)) => 1)
  (check (cmark-node-type (cmark-node-first-child outer)) => 'block_quote)
  (check (cmark-node-parent inner) => outer)
) ;let

(check-report)
