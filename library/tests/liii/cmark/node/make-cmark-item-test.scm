
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

;; make-cmark-item
;; 创建列表项节点。
;;
;; 语法
;; ----
;; (make-cmark-item)

(let ((node (make-cmark-item)))
  (check (cmark-node-type node) => 'item)
  (check (cmark-node-data node) => '())
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试作为容器追加子节点
(let ((item (make-cmark-item)))
  (let ((para (make-cmark-paragraph)))
    (cmark-node-append-child! item para)
    (check (length (cmark-node-children item)) => 1)
    (check (cmark-node-parent para) => item)
    (check (cmark-node-type (cmark-node-first-child item)) => 'paragraph)
  ) ;let
) ;let

;; 测试追加多个子节点
(let ((item (make-cmark-item)))
  (let ((p1 (make-cmark-paragraph)) (p2 (make-cmark-paragraph)))
    (cmark-node-append-child! item p1)
    (cmark-node-append-child! item p2)
    (check (length (cmark-node-children item)) => 2)
    (check (cmark-node-first-child item) => p1)
    (check (cmark-node-last-child item) => p2)
  ) ;let
) ;let

(check-report)
