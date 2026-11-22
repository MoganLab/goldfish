;; make-cmark-math-inline
;; 创建行内数学公式节点。
;;
;; 语法
;; ----
;; (make-cmark-math-inline literal)
;;
;; 参数
;; ----
;; literal : any
;; 输入参数。
;;
;; 返回值
;; ----
;; cmark-node
;; 返回新创建的 cmark AST 节点。

(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

(let ((node (make-cmark-math-inline "x^2")))
  (check (cmark-node-type node) => 'math_inline)
  (check (assoc 'literal (cmark-node-data node)) => '(literal . "x^2"))
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试空公式
(let ((node (make-cmark-math-inline "")))
  (check (assoc 'literal (cmark-node-data node)) => '(literal . ""))
) ;let

;; 测试复杂公式
(let ((node (make-cmark-math-inline "\\frac{a}{b}")))
  (check (assoc 'literal (cmark-node-data node)) => '(literal . "\\frac{a}{b}"))
) ;let

(check-report)
