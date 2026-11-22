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

(check-report)
