(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

;; make-cmark-html-block
;; 创建块级 HTML 节点。
;;
;; 语法
;; ----
;; (make-cmark-html-block literal)

(let ((node (make-cmark-html-block "<div>")))
  (check (cmark-node-type node) => 'html_block)
  (check (assoc 'literal (cmark-node-data node)) => '(literal . "<div>"))
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试空 literal
(let ((node (make-cmark-html-block "")))
  (check (assoc 'literal (cmark-node-data node)) => '(literal . ""))
) ;let

;; 测试多行块级 HTML
(let ((node (make-cmark-html-block "<div>\n  <p>Text</p>\n</div>")))
  (check (assoc 'literal (cmark-node-data node))
    =>
    '(literal . "<div>\n  <p>Text</p>\n</div>")
  ) ;check
) ;let

(check-report)
