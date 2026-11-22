;; render-inlines
;; 普通行内位置递归渲染行内内容项为 Markdown 字符串。
;;
;; 语法
;; ----
;; (render-inlines items)
;;
;; 参数
;; ----
;; items : list
;; 字符串与行内节点混合的列表。
;;
;; 返回值
;; ----
;; string
;; 渲染得到的 Markdown 行内文本。

(import (scheme base))
(import (liii check))
(import (liii cmark))
(import (liii cmark markdown inline))

(check-set-mode! 'report-failed)

(check-true (procedure? render-inlines))

;; ---- 普通文本与转义 ----
(check (render-inlines '()) => "")
(check (render-inlines '("普通文本")) => "普通文本")
(check (render-inlines '("a*b" "c")) => "a\\*bc")
(check (render-inlines '("反引号`代码`")) => "反引号\\`代码\\`")
(check (render-inlines '("文本[方括号]")) => "文本\\[方括号\\]")

;; ---- 强调家族 ----
(check (render-inlines '((em "斜体"))) => "*斜体*")
(check (render-inlines '((strong "粗体"))) => "**粗体**")
(check (render-inlines '((strikeout "删除"))) => "~~删除~~")
(check (render-inlines '((em "a" (strong "b") "c"))) => "*a**b**c*")
(check (render-inlines '("前" (em "中") "后")) => "前*中*后")

;; ---- 行内代码 ----
(check (render-inlines '((code "gf test"))) => "`gf test`")
(check (render-inlines '((code "`x`"))) => "`` `x` ``")
(check (render-inlines '((code "a`b"))) => "``a`b``")
(check (render-inlines '((code " a "))) => "`  a  `")
(check (render-inlines '((code ""))) => "``")

;; ---- 换行 ----
(check (render-inlines '("a" (soft-break) "b")) => "a b")
(check (render-inlines '("a" (line-break) "b")) => "a  b")

;; ---- 链接与图片 ----
(check (render-inlines '((link "https://example.com" "" "链接")))
  => "[链接](https://example.com)"
) ;check
(check (render-inlines '((link "u(v)" "" "文本"))) => "[文本](u\\(v\\))")
(check (render-inlines '((link "u v" "标题" "文本"))) => "[文本](<u v> \"标题\")")
(check (render-inlines '((link "u" "ti\"tle" "文本")))
  => "[文本](u \"ti\\\"tle\")"
) ;check
(check (render-inlines '((image "img.png" "标题" "替代")))
  => "![替代](img.png \"标题\")"
) ;check
(check (render-inlines '((image "i.png" "" ""))) => "![](i.png)")
(check (render-inlines '((image "i.png" "" "图 " (math-inline "x"))))
  => "![图 $x$](i.png)"
) ;check
(check (render-inlines '((image "i.png" "" (emoji "smile"))))
  => "![:smile:](i.png)"
) ;check

;; ---- 数学、HTML、Emoji ----
(check (render-inlines '((math-inline "x^2"))) => "$x^2$")
(check (render-inlines '((math-inline "a$b"))) => "\\(a$b\\)")
(check (render-inlines '((raw-inline "html" "<br>"))) => "<br>")
(check (render-inlines '((emoji "fire"))) => ":fire:")

;; ---- 嵌套结构 ----
(check (render-inlines '((em "a " (code "b") " c"))) => "*a `b` c*")
(check (render-inlines '((strong (link "u" "" "链接文本")))) => "**[链接文本](u)**")
(check (render-inlines '((link "u" "" (image "i.png" "" "图"))))
  => "[![图](i.png)](u)"
) ;check

;; ---- 语义往返：渲染结果重新解析应还原行内结构 ----
(define (inline-roundtrip items)
  (let ((md (render-inlines items)))
    (cdr (car (cdr (markdown->md-stem (string-append "X" md "X")))))
  ) ;let
) ;define

(check (inline-roundtrip '("a*b")) => '("Xa*bX"))
(check (inline-roundtrip '((em "斜体"))) => '("X" (em "斜体") "X"))
(check (inline-roundtrip '((strong "粗体"))) => '("X" (strong "粗体") "X"))
(check (inline-roundtrip '((strikeout "删除"))) => '("X" (strikeout "删除") "X"))
(check (inline-roundtrip '((code "`x`"))) => '("X" (code "`x`") "X"))
(check (inline-roundtrip '((code " a "))) => '("X" (code " a ") "X"))
(check (inline-roundtrip '((code "a`b"))) => '("X" (code "a`b") "X"))
(check (inline-roundtrip '((math-inline "x^2"))) => '("X" (math-inline "x^2") "X"))
(check (inline-roundtrip '((math-inline "a$b"))) => '("X" (math-inline "a$b") "X"))
(check (inline-roundtrip '((emoji "fire"))) => '("X" (emoji "fire") "X"))
(check (inline-roundtrip '((link "u(v)" "" "文本")))
  => '("X" (link "u(v)" "" "文本") "X")
) ;check
(check (inline-roundtrip '((image "img.png" "标题" "替代")))
  => '("X" (image "img.png" "标题" "替代") "X")
) ;check
(check (inline-roundtrip '((raw-inline "html" "<br>")))
  => '("X" (raw-inline "html" "<br>") "X")
) ;check
(check (inline-roundtrip '("文本[方括号]")) => '("X文本[方括号]X"))
(check (inline-roundtrip '("反引号`代码`")) => '("X反引号`代码`X"))

;; ---- 扩展语法文本的语义往返：不会被重新识别为扩展节点 ----
(check (inline-roundtrip '("$x$")) => '("X$x$X"))
(check (inline-roundtrip '("~~删除~~")) => '("X~~删除~~X"))
(check (inline-roundtrip '(":fire:")) => '("X:fire:X"))
(check (inline-roundtrip '("看 https://example.com 链接"))
  => '("X看 https://example.com 链接X")
) ;check
(check (inline-roundtrip '("价格 5$ 和 6$")) => '("X价格 5$ 和 6$X"))
(check (inline-roundtrip '("a~~b~~c")) => '("Xa~~b~~cX"))
(check (inline-roundtrip '("时间 12:30")) => '("X时间 12:30X"))

;; ---- 非法输入 ----
(check-catch 'md-stem-error (render-inlines '((unknown-inline "x"))))
(check-catch 'md-stem-error (render-inlines '((code 42))))
(check-catch 'md-stem-error (render-inlines '(42)))

(check-report)
