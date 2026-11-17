
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark block-parser) (liii cmark node))

(check-set-mode! 'report-failed)

;; start-new-block
;; 根据单行输入选择合适的块级节点构造逻辑。
;;
;; 语法
;; ----
;; (start-new-block line)

(check (cmark-node-type (start-new-block "# Hello")) => 'heading)
(check (cmark-node-type (start-new-block "> quote")) => 'block_quote)
(check (cmark-node-type (start-new-block "- item")) => 'list)
(check
  (cdr (assoc 'start (cmark-node-data (start-new-block "3. item"))))
  =>
  3
) ;check
(check
  (cdr (assoc 'delimiter (cmark-node-data (start-new-block "3) item"))))
  =>
  'one-paren
) ;check
(check (cmark-node-type (start-new-block "---")) => 'thematic_break)
(check (cmark-node-type (start-new-block "  ---")) => 'thematic_break)
(check (cmark-node-type (start-new-block "- - -")) => 'thematic_break)
(check (cmark-node-type (start-new-block "> [!NOTE]")) => 'alert)
(check (cmark-node-type (start-new-block "$$")) => 'math_block)
(check (cmark-node-type (start-new-block "```math")) => 'math_block)
(check (cmark-node-type (start-new-block "\\[")) => 'math_block)
(check (cmark-node-type (start-new-block "\\[x+y\\]")) => 'math_block)
(check (cmark-node-type (start-new-block "Plain paragraph")) => 'paragraph)

(check-report)
