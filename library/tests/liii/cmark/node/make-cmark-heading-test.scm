(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

;; make-cmark-heading
;; 创建带 level 数据的标题节点。
;;
;; 语法
;; ----
;; (make-cmark-heading level)
;;
;; 返回值
;; ----
;; cmark-node
;; 返回类型为 'heading 的节点，并在 data 中保存标题级别。

(let ((node (make-cmark-heading 2)))
  (check (cmark-node-type node) => 'heading)
  (check (assoc 'level (cmark-node-data node)) => '(level . 2))
  (check-true (cmark-heading? node))
) ;let

;; 测试 level 1 到 6 的合法标题级别
(let ((h1 (make-cmark-heading 1)))
  (check (cdr (assoc 'level (cmark-node-data h1))) => 1)
) ;let

(let ((h3 (make-cmark-heading 3)))
  (check (cdr (assoc 'level (cmark-node-data h3))) => 3)
) ;let

(let ((h4 (make-cmark-heading 4)))
  (check (cdr (assoc 'level (cmark-node-data h4))) => 4)
) ;let

(let ((h5 (make-cmark-heading 5)))
  (check (cdr (assoc 'level (cmark-node-data h5))) => 5)
) ;let

(let ((h6 (make-cmark-heading 6)))
  (check (cdr (assoc 'level (cmark-node-data h6))) => 6)
) ;let

;; 测试非法级别：当前构造函数不做验证，仍原样存储
(let ((h0 (make-cmark-heading 0)))
  (check (cdr (assoc 'level (cmark-node-data h0))) => 0)
) ;let

(let ((h7 (make-cmark-heading 7)))
  (check (cdr (assoc 'level (cmark-node-data h7))) => 7)
) ;let

;; 测试初始无子节点与无父节点
(let ((node (make-cmark-heading 2)))
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

(check-report)
