;; parse-frontmatter
;; 解析 Markdown 文件顶部的 YAML frontmatter，返回 (values alist body)；
;; alist 键为字符串；disable-model-invocation 自动转布尔值。
;;
;; 语法
;; ----
;; (parse-frontmatter content) -> values
;;
;; 参数
;; ----
;; content : string
;; 文件完整内容。
;;
;; 返回值
;; ------
;; values
;; (元数据 alist . 正文 markdown body)；无 frontmatter 时元数据为 '()。

(import (scheme base)
  (liii check)
  (liii pi code core skills frontmatter)
) ;import

(check-set-mode! 'report-failed)

;; 标准 frontmatter 解析与布尔转换
(let-values (((fm body)
              (parse-frontmatter "---
name: pdf-tools
description: 提取 PDF 文本
disable-model-invocation: true
---

# PDF 指南
处理文档。")))
  (check (cdr (assoc "name" fm)) => "pdf-tools")
  (check (cdr (assoc "description" fm)) => "提取 PDF 文本")
  (check (cdr (assoc "disable-model-invocation" fm)) => #t)
  (check body => "# PDF 指南\n处理文档。"))

;; 布尔值转换 (false)
(let-values (((fm body) (parse-frontmatter "---
name: admin
description: admin task
disable-model-invocation: false
---
body")))
  (check (cdr (assoc "disable-model-invocation" fm)) => #f))

;; 引号剥除 ("..." 与 '...')
(let-values (((fm body) (parse-frontmatter "---
name: \"quoted-name\"
description: 'quoted description'
---
body")))
  (check (cdr (assoc "name" fm)) => "quoted-name")
  (check (cdr (assoc "description" fm)) => "quoted description"))

;; 多行文本描述 (| 与缩进)
(let-values (((fm body) (parse-frontmatter "---
name: multiline
description: |
  Line 1 of description
  Line 2 of description
---
markdown body")))
  (check (cdr (assoc "description" fm))
    => "Line 1 of description\nLine 2 of description"
  ) ;check
  (check body => "markdown body"))

;; 无 frontmatter 内容
(let-values (((fm body) (parse-frontmatter "# Just Markdown\nNo frontmatter here.")))
  (check fm => '())
  (check body => "# Just Markdown\nNo frontmatter here."))

;; 破损 frontmatter（无闭合 ---）：原样返回
(let-values (((fm body) (parse-frontmatter "---
name: unclosed
description: no closing tag
Just content")))
  (check fm => '()))

(check-report)
