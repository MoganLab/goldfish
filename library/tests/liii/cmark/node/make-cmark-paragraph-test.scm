(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

;; make-cmark-paragraph
;; 创建段落节点。
;;
;; 语法
;; ----
;; (make-cmark-paragraph)
;;
;; 返回值
;; ----
;; cmark-node
;; 返回类型为 'paragraph、且初始无子节点的段落节点。

(let ((node (make-cmark-paragraph)))
  (check (cmark-node-type node) => 'paragraph)
  (check (length (cmark-node-children node)) => 0)
  (check-true (cmark-paragraph? node))
  (check (cmark-node-data node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试追加子节点后的结构
(let ((para (make-cmark-paragraph)))
  (let ((t1 (make-cmark-text "Hello ")) (t2 (make-cmark-text "world")))
    (cmark-node-append-child! para t1)
    (cmark-node-append-child! para t2)
    (check (length (cmark-node-children para)) => 2)
    (check (cmark-node-first-child para) => t1)
    (check (cmark-node-last-child para) => t2)
    (check (cmark-node-parent t1) => para)
    (check (cmark-node-parent t2) => para)
  ) ;let
) ;let

;; 测试 paragraph 作为 block_quote 的子节点
(let ((bq (make-cmark-block-quote)) (para (make-cmark-paragraph)))
  (cmark-node-append-child! bq para)
  (check (cmark-node-parent para) => bq)
  (check (cmark-node-type (cmark-node-first-child bq)) => 'paragraph)
) ;let

(check-report)
