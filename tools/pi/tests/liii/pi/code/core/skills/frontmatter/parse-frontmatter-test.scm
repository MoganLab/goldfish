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

(import (scheme base) (liii check) (liii pi code core skills frontmatter))

(check-set-mode! 'report-failed)

;; 标准 frontmatter 解析与布尔转换
(let-values (((fm body)
              (parse-frontmatter "---\nname: pdf-tools\ndescription: 提取 PDF 文本\ndisable-model-invocation: true\n---\n\n# PDF 指南\n处理文档。"
              ) ;parse-frontmatter
             ) ;
            ) ;
  (check (cdr (assoc "name" fm)) => "pdf-tools")
  (check (cdr (assoc "description" fm)) => "提取 PDF 文本")
  (check (cdr (assoc "disable-model-invocation" fm)) => #t)
  (check body => "# PDF 指南\n处理文档。")
) ;let-values

;; 布尔值转换 (false)
(let-values (((fm body)
              (parse-frontmatter "---\nname: admin\ndescription: admin task\ndisable-model-invocation: false\n---\nbody"
              ) ;parse-frontmatter
             ) ;
            ) ;
  (check (cdr (assoc "disable-model-invocation" fm)) => #f)
) ;let-values

;; 引号剥除 ("..." 与 '...')
(let-values (((fm body)
              (parse-frontmatter "---\nname: \"quoted-name\"\ndescription: 'quoted description'\n---\nbody"
              ) ;parse-frontmatter
             ) ;
            ) ;
  (check (cdr (assoc "name" fm)) => "quoted-name")
  (check (cdr (assoc "description" fm)) => "quoted description")
) ;let-values

;; 多行文本描述 (| 与缩进)
(let-values (((fm body)
              (parse-frontmatter "---\nname: multiline\ndescription: |\n  Line 1 of description\n  Line 2 of description\n---\nmarkdown body"
              ) ;parse-frontmatter
             ) ;
            ) ;
  (check (cdr (assoc "description" fm))
    =>
    "Line 1 of description\nLine 2 of description"
  ) ;check
  (check body => "markdown body")
) ;let-values

;; 无 frontmatter 内容
(let-values (((fm body) (parse-frontmatter "# Just Markdown\nNo frontmatter here.")))
  (check fm => '())
  (check body => "# Just Markdown\nNo frontmatter here.")
) ;let-values

;; 破损 frontmatter（无闭合 ---）：原样返回
(let-values (((fm body)
              (parse-frontmatter "---\nname: unclosed\ndescription: no closing tag\nJust content"
              ) ;parse-frontmatter
             ) ;
            ) ;
  (check fm => '())
) ;let-values

(check-report)
