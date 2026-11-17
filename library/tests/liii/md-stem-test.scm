
(set! *load-path* (cons "markdown/goldfish" *load-path*))

;; (liii cmark stem) 模块函数分类索引
;;
;; md-stem 提供 Markdown 到语义 S 表达式的转换入口。

;; ==== 常见用法示例 ====
(import (liii cmark stem))

(markdown->md-stem "# 标题")
(markdown->md-stem "正文")
(markdown->md-stem "运行`gf test`")
(markdown->md-stem "第一行\n第二行")
(markdown->md-stem "第一行  \n第二行")
(markdown->md-stem "> 引用")
(markdown->md-stem "- 第一项\n- 第二项")
(markdown->md-stem "- [ ] 待办\n- [x] 完成")
(markdown->md-stem "3. 第三项\n4. 第四项")
(markdown->md-stem "1) 第一项\n2) 第二项")
(markdown->md-stem "前文\n\n---\n\n后文")
(markdown->md-stem "| Name | Value |\n| --- | --- |\n| A | 1 |")
(markdown->md-stem "公式是 $x^2$")
(markdown->md-stem "$$\nE = mc^2\n$$")
(markdown->md-stem "~~已删除~~")
(markdown->md-stem "访问 https://example.com")
(markdown->md-stem ":smile:")
(markdown->md-stem "前文<span>后文")
(markdown->md-stem "<div>\ncontent\n</div>")
(markdown->md-stem "> [!NOTE]\n> 提示内容")

;; ==== 函数分类索引 ====
;;
;; 一、转换接口
;;   markdown->md-stem - 把 Markdown 字符串转换为 md-stem S 表达式
