;; markdown->md-stem
;; 把 Markdown 文本转换成 md-stem S 表达式。
;;
;; 语法
;; ----
;; (markdown->md-stem markdown)
;;
;; 参数
;; ----
;; markdown : string
;; 输入的 Markdown 文本字符串。
;;
;; 返回值
;; ----
;; pair
;; 返回形如 (md-stem ...) 的 S 表达式文档树。

(import (scheme base))
(import (liii check))
(import (liii cmark stem))

(check-set-mode! 'report-failed)

(check (markdown->md-stem "# 标题") => '(md-stem (h1 "标题")))
(check (markdown->md-stem "正文") => '(md-stem (p "正文")))
(check (markdown->md-stem "运行 `gf test`")
  =>
  '(md-stem (p "运行 " (code "gf test")))
) ;check
(check (markdown->md-stem "**粗体**与*斜体*")
  =>
  '(md-stem (p (strong "粗体") "与" (em "斜体")))
) ;check
(check (markdown->md-stem "> 引用") => '(md-stem (blockquote (p "引用"))))
(check (markdown->md-stem "- 第一项\n- 第二项")
  =>
  '(md-stem (bullet-list tight (item (p "第一项")) (item (p "第二项"))))
) ;check
(check (markdown->md-stem "公式是 $x^2$")
  =>
  '(md-stem (p "公式是 " (math-inline "x^2")))
) ;check
(check (markdown->md-stem "$$\nE = mc^2\n$$")
  =>
  '(md-stem (math-block "E = mc^2"))
) ;check
(check (markdown->md-stem "~~已删除~~")
  =>
  '(md-stem (p (strikeout "已删除")))
) ;check
(check (markdown->md-stem "> [!NOTE]\n> 提示内容")
  =>
  '(md-stem (alert note (p "提示内容")))
) ;check

(check-report)
