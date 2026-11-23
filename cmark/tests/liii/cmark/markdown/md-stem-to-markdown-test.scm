;; md-stem->markdown
;; 把 md-stem S 表达式文档反向渲染为 Markdown 文本字符串。
;;
;; 语法
;; ----
;; (md-stem->markdown doc)
;;
;; 参数
;; ----
;; doc : pair
;; 形如 (md-stem 块 ...) 的完整 md-stem 文档 S 表达式。
;;
;; 返回值
;; ----
;; string
;; 渲染得到的 Markdown 文本；顶层块以空行分隔，统一使用 \n 换行并以单个换行结尾；空文档返回空字符串。
;;
;; 错误处理
;; ----
;; 输入缺少 md-stem 文档头、节点类型未知或字段形状非法时，抛出 md-stem-error。

(import (scheme base))
(import (liii check))
(import (liii cmark))

(check-set-mode! 'report-failed)

(check-true (procedure? md-stem->markdown))

;; 1. 空文档
(check (md-stem->markdown '(md-stem)) => "")

;; 2. 缺少 md-stem 文档头
(check-catch 'md-stem-error (md-stem->markdown '(p "正文")))
(check-catch 'md-stem-error (md-stem->markdown "字符串不是文档"))

;; 3. 文档不是列表
(check-catch 'md-stem-error (md-stem->markdown 42))
(check-catch 'md-stem-error (md-stem->markdown '(1 2 3)))

;; 4. 未知块节点
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (unknown-block "x"))))

;; 5. 未知行内节点（经段落行内内容校验报错）
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (p (unknown-inline "x")))))

;; 6. 块节点字段数量错误
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (thematic-break "多余字段"))))
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (code-block "info"))))
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (math-block "x" "多余"))))

;; 7. 块节点字段类型错误
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (code-block 123 "content"))))
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (code-block "info" not-a-string))))
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (math-block 42))))

;; 8. 容器枚举值错误
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (bullet-list fat (item)))))
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (ordered-list 1 dot tight (item)))))
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (ordered-list -1 period tight (item)))))
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (alert note? (p "x")))))

;; 9. 容器字段缺失
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (bullet-list))))
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (ordered-list 1 period))))
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (alert))))

;; 10. 容器子节点是非法块
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (blockquote (no-such-block)))))
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (bullet-list tight (item (bad-node))))))

;; 11. 表格结构错误
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (table))))
(check-catch 'md-stem-error
  (md-stem->markdown '(md-stem (table (align default) (thead) (tbody))))
) ;check-catch
(check-catch 'md-stem-error
  (md-stem->markdown
    '(md-stem (table (align bogus) (thead (tr (th "a"))) (tbody)))
  ) ;md-stem->markdown
) ;check-catch
(check-catch 'md-stem-error
  (md-stem->markdown
    '(md-stem (table (align default) (thead (tr (td "a"))) (tbody)))
  ) ;md-stem->markdown
) ;check-catch

;; 12. 行内节点字段错误
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (p (code)))))
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (p (code 42)))))
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (p (link "url")))))
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (p (link 1 2 "text")))))
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (p (raw-inline "html")))))
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (p (emoji :not-string)))))
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (p (soft-break "多余")))))

;; 13. 非列表输入
(check-catch 'md-stem-error (md-stem->markdown '(md-stem 42)))
(check-catch 'md-stem-error (md-stem->markdown '(md-stem . tail)))

;; ---- ST3：叶块与文档输出 ----
;; 14. 文档末尾换行规则：非空文档以单个换行结尾
(check (md-stem->markdown '(md-stem (p "正文"))) => "正文\n")
(check (md-stem->markdown '(md-stem)) => "")

;; 15. 段落
(check (md-stem->markdown '(md-stem (p "正文"))) => "正文\n")
(check (md-stem->markdown '(md-stem (p "a" (strong "b")))) => "a**b**\n")
;; soft-break 渲染为换行（多行段落），语义往返保留结构
(check (md-stem->markdown '(md-stem (p "第一段" (soft-break) "第二段")))
  => "第一段\n第二段\n"
) ;check

;; 16. 标题 h1 至 h6
(check (md-stem->markdown '(md-stem (h1 "一"))) => "# 一\n")
(check (md-stem->markdown '(md-stem (h6 "六"))) => "###### 六\n")
(check (md-stem->markdown '(md-stem (h1))) => "# \n")

;; 17. 分隔线
(check (md-stem->markdown '(md-stem (thematic-break))) => "---\n")

;; 18. 相邻块空行
(check (md-stem->markdown '(md-stem (p "a") (h1 "b") (p "c")))
  => "a\n\n# b\n\nc\n"
) ;check

;; 19. 围栏代码块：语言信息、空内容、含 fence 内容
(check (md-stem->markdown '(md-stem (code-block "ruby" "def foo")))
  => "```ruby\ndef foo\n```\n"
) ;check
(check (md-stem->markdown '(md-stem (code-block "" ""))) => "```\n```\n")
(check (md-stem->markdown '(md-stem (code-block "" "```\ninside")))
  => "````\n```\ninside\n````\n"
) ;check
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (code-block "a`b" "x"))))

;; 20. 数学块
(check (md-stem->markdown '(md-stem (math-block "E = mc^2")))
  => "$$\nE = mc^2\n$$\n"
) ;check
(check-catch 'md-stem-error (md-stem->markdown '(md-stem (math-block "a$$b"))))

;; 21. 原始 HTML 块
(check (md-stem->markdown '(md-stem (raw-block "html" "<div>\nfoo\n</div>")))
  => "<div>\nfoo\n</div>\n"
) ;check

;; 22. 块首特殊文本
(check (md-stem->markdown '(md-stem (p "#不是标题"))) => "\\#不是标题\n")
(check (md-stem->markdown '(md-stem (p "- 列表？"))) => "\\- 列表？\n")
(check (md-stem->markdown '(md-stem (p "1. 编号？"))) => "1\\. 编号？\n")

;; 23. 叶块文档语义往返
(define (leaf-roundtrip doc)
  (markdown->md-stem (md-stem->markdown doc))
) ;define
(check (leaf-roundtrip '(md-stem (p "正文") (h1 "标题")))
  => '(md-stem (p "正文") (h1 "标题"))
) ;check
(check (leaf-roundtrip '(md-stem (code-block "ruby" "def foo\n  1\nend")))
  => '(md-stem (code-block "ruby" "def foo\n  1\nend"))
) ;check
(check (leaf-roundtrip '(md-stem (code-block "" "```\ninside")))
  => '(md-stem (code-block "" "```\ninside"))
) ;check
;; 代码块内容末尾空行保留（末尾 \n 不丢）
(check (leaf-roundtrip '(md-stem (code-block "" "abc\n")))
  => '(md-stem (code-block "" "abc\n"))
) ;check
(check (leaf-roundtrip '(md-stem (code-block "" "\nabc")))
  => '(md-stem (code-block "" "\nabc"))
) ;check
(check (leaf-roundtrip '(md-stem (math-block "E = mc^2")))
  => '(md-stem (math-block "E = mc^2"))
) ;check
(check (leaf-roundtrip '(md-stem (raw-block "html" "<div>\nfoo\n</div>")))
  => '(md-stem (raw-block "html" "<div>\nfoo\n</div>"))
) ;check
(check (leaf-roundtrip '(md-stem (p "a") (thematic-break) (p "b")))
  => '(md-stem (p "a") (thematic-break) (p "b"))
) ;check
(check (leaf-roundtrip '(md-stem (h1 "a*b") (h2 "c`d") (p "e[f]g")))
  => '(md-stem (h1 "a*b") (h2 "c`d") (p "e[f]g"))
) ;check
(check (leaf-roundtrip '(md-stem (p "第一段" (soft-break) "#第二行")))
  => '(md-stem (p "第一段" (soft-break) "#第二行"))
) ;check
(check (leaf-roundtrip '(md-stem (p "a" (line-break) "b")))
  => '(md-stem (p "a" (line-break) "b"))
) ;check

;; ---- 相邻强调定界冲突修复：段落路径完整往返 ----
(check (leaf-roundtrip '(md-stem (p (em "a") (em "b"))))
  => '(md-stem (p (em "a") (em "b")))
) ;check
(check (leaf-roundtrip '(md-stem (p (strong (em "a") (em "b")))))
  => '(md-stem (p (strong (em "a") (em "b"))))
) ;check
(check (leaf-roundtrip
   '(md-stem (p (link "/u" "" (em "a") (em "b")))))
  => '(md-stem (p (link "/u" "" (em "a") (em "b"))))
) ;check
(check (leaf-roundtrip '(md-stem (p (em "a") (em "b") (em "c"))))
  => '(md-stem (p (em "a") (em "b") (em "c")))
) ;check
;; 相邻删除线无法安全往返，统一报 md-stem-error
(check-catch 'md-stem-error
  (md-stem->markdown '(md-stem (p (strikeout "a") (strikeout "b")))))

;; ---- ST4：递归容器 ----
;; 24. 引用块与嵌套
(check (md-stem->markdown '(md-stem (blockquote (p "引用")))) => "> 引用\n")
(check (md-stem->markdown '(md-stem (blockquote (p "a") (p "b"))))
  => "> a\n>\n> b\n"
) ;check
(check (md-stem->markdown '(md-stem (blockquote (blockquote (p "deep")))))
  => "> > deep\n"
) ;check

;; 25. Alert
(check (md-stem->markdown '(md-stem (alert note (p "提示"))))
  => "> [!NOTE]\n> 提示\n"
) ;check
(check (md-stem->markdown '(md-stem (alert tip))) => "> [!TIP]\n")

;; 26. 列表 tight/loose 与编号
(check (md-stem->markdown '(md-stem (bullet-list tight (item (p "a")) (item (p "b")))))
  => "- a\n- b\n"
) ;check
(check (md-stem->markdown '(md-stem (bullet-list loose (item (p "a")) (item (p "b")))))
  => "- a\n\n- b\n"
) ;check
(check
  (md-stem->markdown
    '(md-stem (ordered-list 10 one-paren tight (item (p "a")) (item (p "b")))))
  => "10) a\n11) b\n"
) ;check

;; 27. 任务列表
(check
  (md-stem->markdown
    '(md-stem (bullet-list tight (item checked (p "done")) (item unchecked (p "todo")))))
  => "- [x] done\n- [ ] todo\n"
) ;check

;; 28. 嵌套列表与容器混合
(check
  (md-stem->markdown
    '(md-stem (bullet-list tight (item (p "a") (bullet-list tight (item (p "b")))))))
  => "- a\n  - b\n"
) ;check
(check
  (md-stem->markdown
    '(md-stem
       (ordered-list 1 period tight
         (item (p "a") (ordered-list 1 one-paren tight (item (p "b")))))))
  => "1. a\n   1) b\n"
) ;check

;; 29. 容器语义往返
(define (container-roundtrip doc)
  (markdown->md-stem (md-stem->markdown doc))
) ;define
(check (container-roundtrip '(md-stem (blockquote (p "a") (p "b"))))
  => '(md-stem (blockquote (p "a") (p "b")))
) ;check
(check (container-roundtrip '(md-stem (blockquote (blockquote (p "deep")))))
  => '(md-stem (blockquote (blockquote (p "deep"))))
) ;check
(check (container-roundtrip '(md-stem (alert caution (p "a") (p "b"))))
  => '(md-stem (alert caution (p "a") (p "b")))
) ;check
(check (container-roundtrip '(md-stem (alert tip))) => '(md-stem (alert tip)))
(check (container-roundtrip '(md-stem (bullet-list loose (item (p "a")) (item (p "b")))))
  => '(md-stem (bullet-list loose (item (p "a")) (item (p "b"))))
) ;check
(check
  (container-roundtrip
    '(md-stem (ordered-list 3 one-paren loose (item (p "a")) (item (p "b")))))
  => '(md-stem (ordered-list 3 one-paren loose (item (p "a")) (item (p "b"))))
) ;check
(check
  (container-roundtrip
    '(md-stem (bullet-list tight (item checked (p "done")) (item unchecked (p "todo")))))
  => '(md-stem (bullet-list tight (item checked (p "done")) (item unchecked (p "todo"))))
) ;check
(check
  (container-roundtrip
    '(md-stem (bullet-list tight (item (p "a") (bullet-list tight (item (p "b")))))))
  => '(md-stem (bullet-list tight (item (p "a") (bullet-list tight (item (p "b"))))))
) ;check
(check
  (container-roundtrip
    '(md-stem (ordered-list 1 period loose (item (p "a") (code-block "" "c")))))
  => '(md-stem (ordered-list 1 period loose (item (p "a") (code-block "" "c"))))
) ;check
(check (container-roundtrip '(md-stem (blockquote (p "a") (bullet-list tight (item (p "b"))))))
  => '(md-stem (blockquote (p "a") (bullet-list tight (item (p "b")))))
) ;check
(check
  (container-roundtrip '(md-stem (bullet-list tight (item (blockquote (p "q"))))))
  => '(md-stem (bullet-list tight (item (blockquote (p "q")))))
) ;check

;; ---- ST5：表格 ----

;; 30. 基本表格、四种对齐与空表体
(check
  (md-stem->markdown
    '(md-stem (table (align default) (thead (tr (th "a"))) (tbody))))
  => "|a|\n|---|\n"
) ;check
(check
  (md-stem->markdown
    '(md-stem
       (table (align left center right default)
         (thead (tr (th "a") (th "b") (th "c") (th "d")))
         (tbody (tr (td "1") (td "2") (td "3") (td "4"))))))
  => "|a|b|c|d|\n|:--|:-:|--:|---|\n|1|2|3|4|\n"
) ;check

;; 31. 单元格管道符转义与空单元格
(check
  (md-stem->markdown
    '(md-stem (table (align default) (thead (tr (th "a|b"))) (tbody (tr (td "1"))))))
  => "|a\\|b|\n|---|\n|1|\n"
) ;check
(check
  (md-stem->markdown
    '(md-stem (table (align default default)
                 (thead (tr (th "a") (th)))
                 (tbody (tr (td "1") (td "2"))))))
  => "|a||\n|---|---|\n|1|2|\n"
) ;check

;; 32. 表格语义往返
(define (table-roundtrip doc)
  (markdown->md-stem (md-stem->markdown doc))
) ;define
(check (table-roundtrip '(md-stem (table (align default) (thead (tr (th "a"))) (tbody))))
  => '(md-stem (table (align default) (thead (tr (th "a"))) (tbody)))
) ;check
(check
  (table-roundtrip
    '(md-stem (table (align left center right default)
                 (thead (tr (th "a") (th "b") (th "c") (th "d")))
                 (tbody (tr (td "1") (td "2") (td "3") (td "4"))))))
  => '(md-stem (table (align left center right default)
                   (thead (tr (th "a") (th "b") (th "c") (th "d")))
                   (tbody (tr (td "1") (td "2") (td "3") (td "4")))))
) ;check
(check
  (table-roundtrip
    '(md-stem (table (align default) (thead (tr (th "a|b"))) (tbody (tr (td "1"))))))
  => '(md-stem (table (align default) (thead (tr (th "a|b"))) (tbody (tr (td "1")))))
) ;check
;; code 含管道符：单元格结构保持，\| 留在字面量中（无法语义无损往返）
(check
  (table-roundtrip
    '(md-stem (table (align default) (thead (tr (th "x")))
                 (tbody (tr (td (code "c|d")))))))
  => '(md-stem (table (align default) (thead (tr (th "x")))
                   (tbody (tr (td (code "c\\|d"))))))
) ;check
(check
  (table-roundtrip
    '(md-stem (table (align default default)
                 (thead (tr (th "a") (th "b")))
                 (tbody (tr (td "1") (td)) (tr (td) (td "2"))))))
  => '(md-stem (table (align default default)
                   (thead (tr (th "a") (th "b")))
                   (tbody (tr (td "1") (td)) (tr (td) (td "2")))))
) ;check

;; ---- ST6：autolink / setext 标题 / 空引用块 / 紧凑任务续行 ----
;; 30. 链接自动链接化：url==text 且无 title 输出 <url>
(check (md-stem->markdown '(md-stem (p (link "https://example.com" "" "https://example.com"))))
  => "<https://example.com>\n"
) ;check
;; 邮件链接 url 为 mailto:text 输出 <text>
(check (md-stem->markdown '(md-stem (p (link "mailto:foo@bar.example.com" "" "foo@bar.example.com"))))
  => "<foo@bar.example.com>\n"
) ;check
;; 有 title 不自动链接化
(check (md-stem->markdown '(md-stem (p (link "/url" "标题" "文本"))))
  => "[文本](/url \"标题\")\n"
) ;check
;; url != text 不自动链接化
(check (md-stem->markdown '(md-stem (p (link "/url" "" "文本"))))
  => "[文本](/url)\n"
) ;check

;; 31. 含 soft-break 的 h1/h2 用 setext 下划线渲染
(check (md-stem->markdown '(md-stem (h1 "Foo" (soft-break) "Bar")))
  => "Foo\nBar\n===\n"
) ;check
(check (md-stem->markdown '(md-stem (h2 "Foo" (soft-break) "Bar")))
  => "Foo\nBar\n---\n"
) ;check
;; h3 仍用 ATX（setext 只支持 h1/h2）
(check (md-stem->markdown '(md-stem (h3 "Foo" (soft-break) "Bar")))
  => "### Foo\nBar\n"
) ;check

;; 32. 空引用块渲染为单行 >
(check (md-stem->markdown '(md-stem (blockquote))) => ">\n")
;; 33. 紧凑任务列表多段落续行按基础 marker 宽度缩进
(check (md-stem->markdown
   '(md-stem (bullet-list loose (item unchecked (p "第一段") (p "第二段")))))
  => "- [ ] 第一段\n\n  第二段\n"
) ;check

;; 34. 行首 === 在引用块中被转义为 setext 下划线
(check (md-stem->markdown '(md-stem (blockquote (p "foo" (soft-break) "==="))))
  => "> foo\n> \\===\n"
) ;check

(check-report)
