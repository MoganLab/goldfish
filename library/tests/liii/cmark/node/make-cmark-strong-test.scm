
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

;; make-cmark-strong
;; 创建强强调节点。
;;
;; 语法
;; ----
;; (make-cmark-strong)

(let ((node (make-cmark-strong)))
  (check (cmark-node-type node) => 'strong)
  (check (cmark-node-data node) => '())
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试作为容器追加子节点
(let ((node (make-cmark-strong)))
  (let ((child (make-cmark-text "bold")))
    (cmark-node-append-child! node child)
    (check (length (cmark-node-children node)) => 1)
    (check (cmark-node-type (cmark-node-first-child node)) => 'text)
    (check (cmark-node-parent child) => node)
  ) ;let
) ;let

(check-report)
