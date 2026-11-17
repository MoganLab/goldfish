;; make-cmark-html-inline
;; 创建行内 HTML 节点。
;;
;; 语法
;; ----
;; (make-cmark-html-inline literal)
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

(let ((node (make-cmark-html-inline "<br>")))
  (check (cmark-node-type node) => 'html_inline)
  (check (assoc 'literal (cmark-node-data node)) => '(literal . "<br>"))
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试空 literal
(let ((node (make-cmark-html-inline "")))
  (check (assoc 'literal (cmark-node-data node)) => '(literal . ""))
) ;let

;; 测试复杂行内 HTML
(let ((node (make-cmark-html-inline "<span class='pill' data-x=\"1\">")))
  (check (assoc 'literal (cmark-node-data node))
    =>
    '(literal . "<span class='pill' data-x=\"1\">")
  ) ;check
) ;let

(check-report)
