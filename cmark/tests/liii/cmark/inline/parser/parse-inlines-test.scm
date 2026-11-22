;; parse-inlines
;; 在一段文本里循环识别多种行内语法并生成节点序列。
;;
;; 语法
;; ----
;; (parse-inlines str)
;; (parse-inlines str refmap)
;;
;; 参数
;; ----
;; str : string
;; 待解析的行内文本。
;; refmap : reference-map 或 #f（可选）
;; 链接引用定义映射表。
;;
;; 返回值
;; ----
;; list
;; 返回由 text、code、emph、strong、link、image、html_inline、
;; math_inline、emoji、strikeout、softbreak 或 linebreak 组成的节点列表。

(import (scheme base))
(import (liii check)
  (liii cmark inline parser)
  (liii cmark node)
  (liii cmark block reference)
  (liii unicode)
) ;import

(check-set-mode! 'report-failed)

(define (node-datum node key)
  (cdr (assoc key (cmark-node-data node)))
) ;define

(define (check-single-inline input expected-type key expected-val)
  (let ((nodes (parse-inlines input)))
    (check (length nodes) => 1)
    (check (cmark-node-type (car nodes)) => expected-type)
    (when key
      (check (node-datum (car nodes) key) => expected-val)
    ) ;when
  ) ;let
) ;define

(define (check-mid-inline input expected-len expected-type key expected-val)
  (let ((nodes (parse-inlines input)))
    (check (length nodes) => expected-len)
    (check (cmark-node-type (cadr nodes)) => expected-type)
    (check (node-datum (cadr nodes) key) => expected-val)
  ) ;let
) ;define

(define (check-break-inline input break-type)
  (let ((nodes (parse-inlines input)))
    (check (length nodes) => 3)
    (check (cmark-node-type (car nodes)) => 'text)
    (check (node-datum (car nodes) 'content) => "第一行")
    (check (cmark-node-type (cadr nodes)) => break-type)
    (check (cmark-node-type (caddr nodes)) => 'text)
    (check (node-datum (caddr nodes) 'content) => "第二行")
  ) ;let
) ;define

(check-single-inline "Hello world" 'text #f #f)
(check (>= (length (parse-inlines "**bold** and *italic*")) 3) => #t)
(check (> (length (parse-inlines "Use `code` here")) 1) => #t)
(check-mid-inline "Use `code` here" 3 'code 'literal "code")
(check (> (length (parse-inlines "This is **bold** and *italic* text.")) 1)
  =>
  #t
) ;check
(check (>= (length (parse-inlines "Link: [example](https://example.com) and `code`"))
         3
       ) ;>=
  =>
  #t
) ;check
(check (> (length (parse-inlines "**Bold with *italic* inside**")) 0) => #t)
(check (> (length (parse-inlines "This is **bold** and *italic* text with `code`")) 1)
  =>
  #t
) ;check
(check (> (length (parse-inlines "Formula: $E=mc^2$ is famous")) 1) => #t)

(check-mid-inline "Formula: $E=mc^2$ is famous"
  3
  'math_inline
  'literal
  "E=mc^2"
) ;check-mid-inline
(check-mid-inline "值是 \\(a+b\\) 。" 3 'math_inline 'literal "a+b")
(check-single-inline "值是 \\(a+b 。" 'text #f #f)
(check-mid-inline "公式是 $\\frac{a_1+b^2}{\\sqrt{c}}$。"
  3
  'math_inline
  'literal
  "\\frac{a_1+b^2}{\\sqrt{c}}"
) ;check-mid-inline
(check-mid-inline "矩阵 $\\begin{pmatrix}a&b\\\\c&d\\end{pmatrix}$ 可以行内出现。"
  3
  'math_inline
  'literal
  "\\begin{pmatrix}a&b\\\\c&d\\end{pmatrix}"
) ;check-mid-inline

(let ((nodes (parse-inlines "前~~中间~~后")))
  (check (length nodes) => 3)
  (check (cmark-node-type (cadr nodes)) => 'strikeout)
  (check
    (node-datum (car (cmark-node-children (cadr nodes))) 'content)
    =>
    "中间"
  ) ;check
) ;let

(let ((nodes (parse-inlines "~~值为 $x^2$~~")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'strikeout)
  (check
    (cmark-node-type (car (cmark-node-children (car nodes))))
    =>
    'text
  ) ;check
  (check
    (cmark-node-type (cadr (cmark-node-children (car nodes))))
    =>
    'math_inline
  ) ;check
) ;let

(let ((nodes (parse-inlines "~~[链接](https://example.com)~~")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'strikeout)
  (check
    (cmark-node-type (car (cmark-node-children (car nodes))))
    =>
    'link
  ) ;check
) ;let

(check-single-inline "~~~not strike~~~" 'text 'content "~~~not strike~~~")
(check-mid-inline "Value: ``a ` b``." 3 'code 'literal "a ` b")
(check-single-inline "这里有一个未闭合的`行内代码"
  'text
  'content
  "这里有一个未闭合的`行内代码"
) ;check-single-inline

(check-break-inline "第一行\n第二行" 'softbreak)
(check-break-inline "第一行  \n第二行" 'linebreak)
(check-break-inline "第一行\\\n第二行" 'linebreak)

(check-single-inline (string-append "路径" "\\" "temp")
  'text
  'content
  "路径\\temp"
) ;check-single-inline
(check-single-inline "\\*hello\\*" 'text 'content "*hello*")
(check-single-inline "\\[text](https://example.com)"
  'text
  'content
  "[text](https://example.com)"
) ;check-single-inline
(check-single-inline "\\<span>" 'text 'content "<span>")
(check-single-inline "\\\\temp" 'text 'content "\\temp")
(check-single-inline "a\\ b" 'text 'content "a\\ b")
(check-single-inline "`\\*`" 'code 'literal "\\*")
(check-single-inline "Tom &amp; Jerry" 'text 'content "Tom & Jerry")

(check-single-inline ":smile:" 'emoji 'shortcode "smile")
(check-mid-inline "前文 :rocket: 后文" 3 'emoji 'shortcode "rocket")
(check-single-inline ":unknown:" 'text 'content ":unknown:")
(let ((nodes (parse-inlines "`:smile:` 与 :smile:")))
  (check (length nodes) => 3)
  (check (cmark-node-type (car nodes)) => 'code)
  (check (cmark-node-type (caddr nodes)) => 'emoji)
) ;let

(check-mid-inline "前文<span>后文" 3 'html_inline 'literal "<span>")
(check-mid-inline "前文<!-- note -->后文"
  3
  'html_inline
  'literal
  "<!-- note -->"
) ;check-mid-inline
(check-mid-inline "前文</span>后文" 3 'html_inline 'literal "</span>")
(check-mid-inline "请访问<https://example.com>"
  2
  'link
  'url
  "https://example.com"
) ;check-mid-inline
(check-mid-inline "请访问 https://example.com 并继续。"
  3
  'link
  'url
  "https://example.com"
) ;check-mid-inline
(check-mid-inline "请查看(https://example.com/docs)。"
  3
  'link
  'url
  "https://example.com/docs"
) ;check-mid-inline
(check-single-inline "abchttps://example.com 不是裸链接。"
  'text
  'content
  "abchttps://example.com 不是裸链接。"
) ;check-single-inline
(check-mid-inline "联系我<user@example.com>"
  2
  'link
  'url
  "mailto:user@example.com"
) ;check-mid-inline
(check-single-inline "这里是\\<span>普通文本"
  'text
  'content
  "这里是<span>普通文本"
) ;check-single-inline
(check-mid-inline "这里有 `<span>` 代码" 3 'code 'literal "<span>")
(check-mid-inline "前文<span title=\"Tom &amp; Jerry\">后文"
  3
  'html_inline
  'literal
  "<span title=\"Tom &amp; Jerry\">"
) ;check-mid-inline
(check-single-inline "前文</span class=\"x\">后文"
  'text
  'content
  "前文</span class=\"x\">后文"
) ;check-single-inline

(let ((nodes (parse-inlines "看[a <span>b</span> c](https://example.com)和![x <span>y</span> z](logo.png)。"
             ) ;parse-inlines
      ) ;nodes
     ) ;
  (check (length nodes) => 5)
  (check (cmark-node-type (cadr nodes)) => 'link)
  (check (cmark-node-type (cadddr nodes)) => 'image)
  (check
    (cmark-node-type (cadr (cmark-node-children (cadr nodes))))
    =>
    'html_inline
  ) ;check
  (check
    (cmark-node-type (cadr (cmark-node-children (cadddr nodes))))
    =>
    'html_inline
  ) ;check
) ;let

(check-single-inline "&#35; 和 &#x23;" 'text 'content "# 和 #")
(check-single-inline "&copy;" 'text 'content (utf8-string (integer->char 169)))
(check-single-inline "a&nbsp;b"
  'text
  'content
  (string-append "a" (utf8-string (integer->char 160)) "b")
) ;check-single-inline
(check-single-inline "&#0; &#x110000;"
  'text
  'content
  (string-append (utf8-string (integer->char 65533)) " &#x110000;")
) ;check-single-inline
(check-single-inline "&unknown; &#xZZ; &amp"
  'text
  'content
  "&unknown; &#xZZ; &amp"
) ;check-single-inline

(let ((refmap (make-reference-map)))
  (reference-map-add-definition! refmap "doc" "https://example.com/doc" "")
  (reference-map-add-definition! refmap "img" "logo.png" "Logo")
  (let ((nodes (parse-inlines "请看[文档][doc]和![Logo][img]。" refmap)))
    (check (length nodes) => 5)
    (check (cmark-node-type (cadr nodes)) => 'link)
    (check (node-datum (cadr nodes) 'url) => "https://example.com/doc")
    (check (cmark-node-type (cadddr nodes)) => 'image)
    (check (node-datum (cadddr nodes) 'url) => "logo.png")
    (check (node-datum (cadddr nodes) 'title) => "Logo")
  ) ;let
) ;let

(check-report)
