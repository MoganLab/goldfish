(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

;; make-cmark-math-block
;; 创建块级数学公式节点。
;;
;; 语法
;; ----
;; (make-cmark-math-block literal)

(let ((node (make-cmark-math-block "E=mc^2")))
  (check (cmark-node-type node) => 'math_block)
  (check (assoc 'literal (cmark-node-data node)) => '(literal . "E=mc^2"))
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试空公式
(let ((node (make-cmark-math-block "")))
  (check (assoc 'literal (cmark-node-data node)) => '(literal . ""))
) ;let

;; 测试多行公式
(let ((node (make-cmark-math-block "\\int_0^1 x^2\\,dx\n\\sum_{i=1}^{n} i")))
  (check (assoc 'literal (cmark-node-data node))
    =>
    '(literal . "\\int_0^1 x^2\\,dx\n\\sum_{i=1}^{n} i")
  ) ;check
) ;let

;; 测试包含特殊字符的公式
(let ((node (make-cmark-math-block "a^2 + b^2 = c^2")))
  (check (assoc 'literal (cmark-node-data node))
    =>
    '(literal . "a^2 + b^2 = c^2")
  ) ;check
) ;let

(check-report)
