(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

;; make-cmark-code
;; 创建行内代码节点。
;;
;; 语法
;; ----
;; (make-cmark-code literal)

(let ((node (make-cmark-code "x")))
  (check (cmark-node-type node) => 'code)
  (check (assoc 'literal (cmark-node-data node)) => '(literal . "x"))
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试空代码内容
(let ((node (make-cmark-code "")))
  (check (assoc 'literal (cmark-node-data node)) => '(literal . ""))
) ;let

;; 测试包含反引号的代码内容
(let ((node (make-cmark-code "a ` b")))
  (check (assoc 'literal (cmark-node-data node)) => '(literal . "a ` b"))
) ;let

;; 测试包含换行符的代码内容
(let ((node (make-cmark-code "line1\nline2")))
  (check (assoc 'literal (cmark-node-data node)) => '(literal . "line1\nline2"))
) ;let

(check-report)
