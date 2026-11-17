(import (liii check) (liii cmark block parser) (liii cmark node))

(check-set-mode! 'report-failed)

;; parse-document-lines
;; 把多行 Markdown 文本按块级规则组织为文档节点。
;;
;; 语法
;; ----
;; (parse-document-lines lines)

(let ((doc (parse-document-lines '("# Hello" "" "World"))))
  (check (cmark-node-type doc) => 'document)
) ;let

(let ((doc (parse-document-lines '("# Heading" "" "Paragraph text"))))
  (check (cmark-node-type doc) => 'document)
  (check (cmark-node-type (car (cmark-node-children doc))) => 'heading)
) ;let

(let ((doc (parse-document-lines '("| Name | Value |"
                                   "| --- | --- |"
                                   "| A | 1 |"))))
  (check (cmark-node-type doc) => 'document)
  (check (cmark-node-type (car (cmark-node-children doc))) => 'table)
) ;let

(let ((doc (parse-document-lines '("<div>" "html block" "</div>" ""
                                   "Paragraph text"))
      ) ;doc
     ) ;
  (check (cmark-node-type doc) => 'document)
  (check (cmark-node-type (car (cmark-node-children doc))) => 'html_block)
  (check
    (cdr
      (assoc 'literal (cmark-node-data (car (cmark-node-children doc))))
    ) ;cdr
    =>
    "<div>\nhtml block\n</div>"
  ) ;check
) ;let

(let ((doc (parse-document-lines '("$$" "E = mc^2" "$$"))))
  (check (cmark-node-type doc) => 'document)
  (check (cmark-node-type (car (cmark-node-children doc))) => 'math_block)
  (check
    (cdr
      (assoc 'literal (cmark-node-data (car (cmark-node-children doc))))
    ) ;cdr
    =>
    "E = mc^2"
  ) ;check
) ;let

(let ((doc (parse-document-lines '("```math" "\\int_0^1 x^2\\,dx" "```"))))
  (check (cmark-node-type doc) => 'document)
  (check (cmark-node-type (car (cmark-node-children doc))) => 'math_block)
  (check
    (cdr
      (assoc 'literal (cmark-node-data (car (cmark-node-children doc))))
    ) ;cdr
    =>
    "\\int_0^1 x^2\\,dx"
  ) ;check
) ;let

(let ((doc (parse-document-lines '("> [!NOTE]" "> 提示内容"))))
  (check (cmark-node-type doc) => 'document)
  (check (cmark-node-type (car (cmark-node-children doc))) => 'alert)
  (check
    (cdr
      (assoc 'kind (cmark-node-data (car (cmark-node-children doc))))
    ) ;cdr
    =>
    'note
  ) ;check
) ;let

;; \[...\] 单行块（0111：解析器原生支持）
(let ((doc (parse-document-lines '("\\[E = mc^2\\]"))))
  (check (cmark-node-type doc) => 'document)
  (check (cmark-node-type (car (cmark-node-children doc))) => 'math_block)
  (check
    (cdr
      (assoc 'literal (cmark-node-data (car (cmark-node-children doc))))
    ) ;cdr
    =>
    "E = mc^2"
  ) ;check
) ;let

;; \[ 多行块
(let ((doc (parse-document-lines '("\\[" "x+y" "\\]"))))
  (check (cmark-node-type (car (cmark-node-children doc))) => 'math_block)
  (check
    (cdr
      (assoc 'literal (cmark-node-data (car (cmark-node-children doc))))
    ) ;cdr
    =>
    "x+y"
  ) ;check
) ;let

(check-report)
