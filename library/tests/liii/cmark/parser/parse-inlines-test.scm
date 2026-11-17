(import (scheme base))
(import (liii check)
  (liii cmark inline parser)
  (liii cmark node)
  (liii cmark block reference)
  (liii unicode)
) ;import

(check-set-mode! 'report-failed)

;; parse-inlines
;; 在一段文本里循环识别多种行内语法并生成节点序列。
;;
;; 语法
;; ----
;; (parse-inlines str)
;;
;; 返回值
;; ----
;; list
;; 返回由 text、code、emph、strong、link、image、html_inline、
;; math_inline、emoji、strikeout、softbreak 或 linebreak 组成的节点列表。

(let ((nodes (parse-inlines "Hello world")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
) ;let

(check (>= (length (parse-inlines "**bold** and *italic*")) 3) => #t)
(check (> (length (parse-inlines "Use `code` here")) 1) => #t)

(let ((nodes (parse-inlines "Use `code` here")))
  (check (cmark-node-type (cadr nodes)) => 'code)
  (check
    (cdr (assoc 'literal (cmark-node-data (cadr nodes))))
    =>
    "code"
  ) ;check
) ;let

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

(let ((nodes (parse-inlines "Formula: $E=mc^2$ is famous")))
  (check (length nodes) => 3)
  (check (cmark-node-type (cadr nodes)) => 'math_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (cadr nodes))))
    =>
    "E=mc^2"
  ) ;check
) ;let

;; \( 是数学扩展定界（0111 起解析器原生支持）：\(a+b\) 为行内公式
(let ((nodes (parse-inlines "值是 \\(a+b\\) 。")))
  (check (length nodes) => 3)
  (check (cmark-node-type (cadr nodes)) => 'math_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (cadr nodes))))
    =>
    "a+b"
  ) ;check
) ;let

;; 未闭合的 \( 回退为转义字面量（CommonMark Example 12/14 语义）
(let ((nodes (parse-inlines "值是 \\(a+b 。")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
) ;let

(let ((nodes (parse-inlines "公式是 $\\frac{a_1+b^2}{\\sqrt{c}}$。")))
  (check (length nodes) => 3)
  (check (cmark-node-type (cadr nodes)) => 'math_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (cadr nodes))))
    =>
    "\\frac{a_1+b^2}{\\sqrt{c}}"
  ) ;check
) ;let

(let ((nodes (parse-inlines "矩阵 $\\begin{pmatrix}a&b\\\\c&d\\end{pmatrix}$ 可以行内出现。"
             ) ;parse-inlines
      ) ;nodes
     ) ;
  (check (length nodes) => 3)
  (check (cmark-node-type (cadr nodes)) => 'math_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (cadr nodes))))
    =>
    "\\begin{pmatrix}a&b\\\\c&d\\end{pmatrix}"
  ) ;check
) ;let

(let ((nodes (parse-inlines "前~~中间~~后")))
  (check (length nodes) => 3)
  (check (cmark-node-type (cadr nodes)) => 'strikeout)
  (check
    (cdr
      (assoc 'content (cmark-node-data (car (cmark-node-children (cadr nodes)))))
    ) ;cdr
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

(let ((nodes (parse-inlines "~~~not strike~~~")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    "~~~not strike~~~"
  ) ;check
) ;let

(let ((nodes (parse-inlines "Value: ``a ` b``.")))
  (check (cmark-node-type (cadr nodes)) => 'code)
  (check
    (cdr (assoc 'literal (cmark-node-data (cadr nodes))))
    =>
    "a ` b"
  ) ;check
) ;let

(let ((nodes (parse-inlines "这里有一个未闭合的`行内代码")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    "这里有一个未闭合的`行内代码"
  ) ;check
) ;let

(let ((nodes (parse-inlines "第一行\n第二行")))
  (check (length nodes) => 3)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    "第一行"
  ) ;check
  (check (cmark-node-type (cadr nodes)) => 'softbreak)
  (check (cmark-node-type (caddr nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (caddr nodes))))
    =>
    "第二行"
  ) ;check
) ;let

(let ((nodes (parse-inlines "第一行  \n第二行")))
  (check (length nodes) => 3)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    "第一行"
  ) ;check
  (check (cmark-node-type (cadr nodes)) => 'linebreak)
  (check (cmark-node-type (caddr nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (caddr nodes))))
    =>
    "第二行"
  ) ;check
) ;let

(let ((nodes (parse-inlines "第一行\\\n第二行")))
  (check (length nodes) => 3)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    "第一行"
  ) ;check
  (check (cmark-node-type (cadr nodes)) => 'linebreak)
  (check (cmark-node-type (caddr nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (caddr nodes))))
    =>
    "第二行"
  ) ;check
) ;let

(let ((nodes (parse-inlines (string-append "路径" "\\" "temp"))))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    "路径\\temp"
  ) ;check
) ;let

(let ((nodes (parse-inlines "\\*hello\\*")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    "*hello*"
  ) ;check
) ;let

(let ((nodes (parse-inlines "\\[text](https://example.com)")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    "[text](https://example.com)"
  ) ;check
) ;let

(let ((nodes (parse-inlines "\\<span>")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    "<span>"
  ) ;check
) ;let

(let ((nodes (parse-inlines "\\\\temp")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    "\\temp"
  ) ;check
) ;let

;; CommonMark: 反斜杠不能转义空格，\ 保留为字面（Example 13）
(let ((nodes (parse-inlines "a\\ b")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    "a\\ b"
  ) ;check
) ;let

(let ((nodes (parse-inlines "`\\*`")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'code)
  (check
    (cdr (assoc 'literal (cmark-node-data (car nodes))))
    =>
    "\\*"
  ) ;check
) ;let

(let ((nodes (parse-inlines "Tom &amp; Jerry")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    "Tom & Jerry"
  ) ;check
) ;let

(let ((nodes (parse-inlines ":smile:")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'emoji)
  (check (assoc 'shortcode (cmark-node-data (car nodes)))
    =>
    '(shortcode . "smile")
  ) ;check
) ;let

(let ((nodes (parse-inlines "前文 :rocket: 后文")))
  (check (length nodes) => 3)
  (check (cmark-node-type (cadr nodes)) => 'emoji)
  (check (assoc 'shortcode (cmark-node-data (cadr nodes)))
    =>
    '(shortcode . "rocket")
  ) ;check
) ;let

(let ((nodes (parse-inlines ":unknown:")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    ":unknown:"
  ) ;check
) ;let

(let ((nodes (parse-inlines "`:smile:` 与 :smile:")))
  (check (length nodes) => 3)
  (check (cmark-node-type (car nodes)) => 'code)
  (check (cmark-node-type (caddr nodes)) => 'emoji)
) ;let

(let ((nodes (parse-inlines "前文<span>后文")))
  (check (length nodes) => 3)
  (check (cmark-node-type (car nodes)) => 'text)
  (check (cmark-node-type (cadr nodes)) => 'html_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (cadr nodes))))
    =>
    "<span>"
  ) ;check
  (check (cmark-node-type (caddr nodes)) => 'text)
) ;let

(let ((nodes (parse-inlines "前文<!-- note -->后文")))
  (check (length nodes) => 3)
  (check (cmark-node-type (cadr nodes)) => 'html_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (cadr nodes))))
    =>
    "<!-- note -->"
  ) ;check
) ;let

(let ((nodes (parse-inlines "前文</span>后文")))
  (check (length nodes) => 3)
  (check (cmark-node-type (cadr nodes)) => 'html_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (cadr nodes))))
    =>
    "</span>"
  ) ;check
) ;let

(let ((nodes (parse-inlines "请访问<https://example.com>")))
  (check (length nodes) => 2)
  (check (cmark-node-type (cadr nodes)) => 'link)
  (check
    (cdr (assoc 'url (cmark-node-data (cadr nodes))))
    =>
    "https://example.com"
  ) ;check
) ;let

(let ((nodes (parse-inlines "请访问 https://example.com 并继续。")))
  (check (length nodes) => 3)
  (check (cmark-node-type (cadr nodes)) => 'link)
  (check
    (cdr (assoc 'url (cmark-node-data (cadr nodes))))
    =>
    "https://example.com"
  ) ;check
) ;let

(let ((nodes (parse-inlines "请查看(https://example.com/docs)。")))
  (check (length nodes) => 3)
  (check (cmark-node-type (cadr nodes)) => 'link)
  (check
    (cdr (assoc 'url (cmark-node-data (cadr nodes))))
    =>
    "https://example.com/docs"
  ) ;check
) ;let

(let ((nodes (parse-inlines "abchttps://example.com 不是裸链接。")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    "abchttps://example.com 不是裸链接。"
  ) ;check
) ;let

(let ((nodes (parse-inlines "联系我<user@example.com>")))
  (check (length nodes) => 2)
  (check (cmark-node-type (cadr nodes)) => 'link)
  (check
    (cdr (assoc 'url (cmark-node-data (cadr nodes))))
    =>
    "mailto:user@example.com"
  ) ;check
) ;let

(let ((nodes (parse-inlines "这里是\\<span>普通文本")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    "这里是<span>普通文本"
  ) ;check
) ;let

(let ((nodes (parse-inlines "这里有 `<span>` 代码")))
  (check (length nodes) => 3)
  (check (cmark-node-type (cadr nodes)) => 'code)
  (check
    (cdr (assoc 'literal (cmark-node-data (cadr nodes))))
    =>
    "<span>"
  ) ;check
) ;let

(let ((nodes (parse-inlines "前文<span title=\"Tom &amp; Jerry\">后文")))
  (check (length nodes) => 3)
  (check (cmark-node-type (cadr nodes)) => 'html_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (cadr nodes))))
    =>
    "<span title=\"Tom &amp; Jerry\">"
  ) ;check
) ;let

(let ((nodes (parse-inlines "前文</span class=\"x\">后文")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    "前文</span class=\"x\">后文"
  ) ;check
) ;let

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

(let ((nodes (parse-inlines "&#35; 和 &#x23;")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    "# 和 #"
  ) ;check
) ;let

(let ((nodes (parse-inlines "&copy;")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    (utf8-string (integer->char 169))
  ) ;check
) ;let

(let ((nodes (parse-inlines "a&nbsp;b")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    (string-append "a" (utf8-string (integer->char 160)) "b")
  ) ;check
) ;let

(let ((nodes (parse-inlines "&#0; &#x110000;")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    ;; &#0; → U+FFFD；&#x110000; 越界原文保留（CommonMark Example 26/28）
    (string-append (utf8-string (integer->char 65533)) " &#x110000;")
  ) ;check
) ;let

(let ((nodes (parse-inlines "&unknown; &#xZZ; &amp")))
  (check (length nodes) => 1)
  (check (cmark-node-type (car nodes)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car nodes))))
    =>
    "&unknown; &#xZZ; &amp"
  ) ;check
) ;let

(let ((refmap (make-reference-map)))
  (reference-map-add-definition! refmap "doc" "https://example.com/doc" "")
  (reference-map-add-definition! refmap "img" "logo.png" "Logo")
  (let ((nodes (parse-inlines "请看[文档][doc]和![Logo][img]。" refmap)))
    (check (length nodes) => 5)
    (check (cmark-node-type (cadr nodes)) => 'link)
    (check
      (cdr (assoc 'url (cmark-node-data (cadr nodes))))
      =>
      "https://example.com/doc"
    ) ;check
    (check (cmark-node-type (cadddr nodes)) => 'image)
    (check
      (cdr (assoc 'url (cmark-node-data (cadddr nodes))))
      =>
      "logo.png"
    ) ;check
  ) ;let
) ;let

(let ((refmap (make-reference-map)))
  (reference-map-add-definition! refmap "my&ref" "https://example.com/entity" "")
  (let ((nodes (parse-inlines "[my&amp;ref][]" refmap)))
    (check (length nodes) => 1)
    (check (cmark-node-type (car nodes)) => 'link)
    (check
      (cdr (assoc 'url (cmark-node-data (car nodes))))
      =>
      "https://example.com/entity"
    ) ;check
    (check
      (cdr
        (assoc 'content (cmark-node-data (car (cmark-node-children (car nodes)))))
      ) ;cdr
      =>
      "my&ref"
    ) ;check
  ) ;let
) ;let

(check-report)
