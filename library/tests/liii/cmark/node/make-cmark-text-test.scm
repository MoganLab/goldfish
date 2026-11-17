(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

;; make-cmark-text
;; 创建文本节点。
;;
;; 语法
;; ----
;; (make-cmark-text content)
;;
;; 返回值
;; ----
;; cmark-node
;; 返回类型为 'text 的节点，并把文本写入 content 字段。

(let ((node (make-cmark-text "Hello")))
  (check (cmark-node-type node) => 'text)
  (check (assoc 'content (cmark-node-data node)) => '(content . "Hello"))
  (check-true (cmark-text? node))
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试空内容
(let ((node (make-cmark-text "")))
  (check (assoc 'content (cmark-node-data node)) => '(content . ""))
) ;let

;; 测试包含空白字符的内容
(let ((node (make-cmark-text "  spaced  ")))
  (check (assoc 'content (cmark-node-data node)) => '(content . "  spaced  "))
) ;let

;; 测试包含换行符的内容
(let ((node (make-cmark-text "line1\nline2")))
  (check (assoc 'content (cmark-node-data node)) => '(content . "line1\nline2"))
) ;let

(check-report)
