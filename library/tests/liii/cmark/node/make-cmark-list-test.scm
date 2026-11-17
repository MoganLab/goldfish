
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

;; make-cmark-list
;; 创建列表节点。
;;
;; 语法
;; ----
;; (make-cmark-list list-type start delimiter)

(let ((node (make-cmark-list 'ordered 1 #\.)))
  (check (cmark-node-type node) => 'list)
  (check (cdr (assoc 'type (cmark-node-data node))) => 'ordered)
  (check (cdr (assoc 'start (cmark-node-data node))) => 1)
  (check-true (cmark-list? node))
) ;let

;; 测试 bullet 类型列表
(let ((node (make-cmark-list 'bullet 0 #f)))
  (check (cmark-node-type node) => 'list)
  (check (cdr (assoc 'type (cmark-node-data node))) => 'bullet)
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试不同 start 值
(let ((node (make-cmark-list 'ordered 5 #\.)))
  (check (cdr (assoc 'start (cmark-node-data node))) => 5)
) ;let

;; 测试 delimiter 为 one-paren（用 #\x29 表示右括号字符）
(let ((node (make-cmark-list 'ordered 10 #\x29)))
  (check (cdr (assoc 'start (cmark-node-data node))) => 10)
  (check (cdr (assoc 'delimiter (cmark-node-data node))) => #\x29)
) ;let

(let ((node (make-cmark-list 'ordered 3 #\x29)))
  (check (cdr (assoc 'delimiter (cmark-node-data node))) => #\x29)
) ;let

(check-report)
