
(set! *load-path* (cons "markdown/goldfish" *load-path*))

;; (liii cmark parser) 模块函数分类索引
;;
;; cmark-parser 负责把 Markdown 文本拆分成块级节点与行内节点，
;; 同时提供一批辅助判断函数，用于识别标题、列表、代码块和内联标记。

;; ==== 常见用法示例 ====
(import (liii cmark block-parser)
  (liii cmark inline-parser)
  (liii utils text-util)
) ;import
(import (liii cmark node))

(cmark-node-type (parse-atx-heading "# Hello"))
(cmark-node-type (parse-paragraph '("Hello world")))
(cmark-node-type (car (parse-inlines "`code`")))

;; ==== 函数分类索引 ====
;;
;; 一、块级判断与解析（(liii cmark block-parser)）
;;   blank-line?
;;   blockquote-line?
;;   unordered-list-item?
;;   ordered-list-item?
;;   fenced-code-start?
;;   thematic-break-line?
;;   paragraph-continues?
;;   parse-paragraph
;;   parse-thematic-break
;;   atx-heading-line?
;;   parse-atx-heading
;;   indented-code-line?
;;   parse-indented-code-block
;;   fenced-code-block-end?
;;   parse-fenced-code-block
;;   parse-list-item
;;   parse-list
;;   start-new-block
;;   parse-document-lines
;;   parse-blockquote
;;   setext-underline?
;;   setext-heading-level
;;   parse-setext-heading
;;   html-block-start?
;;   parse-html-block
;;   block-continues?
;;
;; 二、字符串与缩进工具（(liii utils text-util)）
;;   string-trim-left
;;   string-trim-right
;;   string-trim
;;   count-leading-spaces
;;
;; 三、行内解析器状态（(liii cmark inline-parser)）
;;   make-cmark-inline-parser
;;   cmark-inline-parser?
;;   cmark-inline-parser-content
;;   cmark-inline-parser-set-content!
;;   cmark-inline-parser-pos
;;   cmark-inline-parser-set-pos!
;;   cmark-inline-parser-new
;;
;; 四、行内判断与解析（(liii cmark inline-parser)）
;;   parse-text
;;   inline-code-start?
;;   parse-inline-code
;;   emph-start?
;;   parse-emph
;;   strong-start?
;;   parse-strong
;;   link-start?
;;   parse-link
;;   image-start?
;;   parse-image
;;   autolink-start?
;;   parse-autolink
;;   inline-html-start?
;;   parse-inline-html
;;   linebreak?
;;   parse-linebreak
;;   parse-inlines
