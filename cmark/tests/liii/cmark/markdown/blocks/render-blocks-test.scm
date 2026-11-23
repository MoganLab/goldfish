;; render-blocks
;; 把已校验的块节点列表渲染为按逻辑行组织的字符串列表。
;;
;; 语法
;; ----
;; (render-blocks nodes)
;;
;; 参数
;; ----
;; nodes : list
;; 块节点列表。
;;
;; 返回值
;; ----
;; list
;; 行字符串列表；兄弟块间空行由文档层统一插入。
;;
;; 错误处理
;; ----
;; 节点非法或尚未支持时抛出 md-stem-error。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown blocks))

(check-set-mode! 'report-failed)

(check-true (procedure? render-blocks))
(check-true (procedure? render-block))

;; 空块列表
(check (render-blocks '()) => '())

;; ---- 段落 ----
(check (render-blocks '((p "正文"))) => '("正文"))
(check (render-blocks '((p "a" (em "b")))) => '("a*b*"))
(check (render-blocks '((p "#不是标题"))) => '("\\#不是标题"))

;; ---- 标题 ----
(check (render-blocks '((h1 "标题"))) => '("# 标题"))
(check (render-blocks '((h6 "六级"))) => '("###### 六级"))
(check (render-blocks '((h1))) => '("# "))
(check (render-blocks '((h3 "a*b"))) => '("### a\\*b"))

;; ---- 分隔线 ----
(check (render-blocks '((thematic-break))) => '("---"))

;; ---- 围栏代码块 ----
(check (render-blocks '((code-block "ruby" "def foo")))
  => '("```ruby" "def foo" "```")
) ;check
(check (render-blocks '((code-block "" ""))) => '("```" "```"))
(check (render-blocks '((code-block "" "```\ninside")))
  => '("````" "```" "inside" "````")
) ;check
;; info 含反引号报错
(check-catch 'md-stem-error (render-blocks '((code-block "a`b" "x"))))

;; ---- 数学块 ----
(check (render-blocks '((math-block "E"))) => '("$$" "E" "$$"))
(check (render-blocks '((math-block ""))) => '("$$" "$$"))
;; 内容含 $$ 报错
(check-catch 'md-stem-error (render-blocks '((math-block "a$$b"))))

;; ---- 原始 HTML 块 ----
(check (render-blocks '((raw-block "html" "<div>\nfoo\n</div>")))
  => '("<div>" "foo" "</div>")
) ;check
(check (render-blocks '((raw-block "html" "<br>"))) => '("<br>"))

;; ---- 多块拼接（无空行，由文档层插入） ----
(check (render-blocks '((p "a") (p "b"))) => '("a" "b"))

;; ---- 容器（引用、Alert、列表） ----
(check (render-blocks '((blockquote (p "引用")))) => '("> 引用"))
(check (render-blocks '((alert note (p "提示"))))
  => '("> [!NOTE]" "> 提示")
) ;check
(check (render-blocks '((bullet-list tight (item (p "a"))))) => '("- a"))
(check (render-blocks '((ordered-list 1 period tight (item (p "a")))))
  => '("1. a")
) ;check

;; ---- 表格 ----
(check
  (render-blocks
    '((table (align default) (thead (tr (th "a"))) (tbody))))
  => '("|a|" "|---|")
) ;check

;; ---- 非法节点 ----
(check-catch 'md-stem-error (render-blocks '(42)))
(check-catch 'md-stem-error (render-blocks '((unknown-block "x"))))

(check-report)
