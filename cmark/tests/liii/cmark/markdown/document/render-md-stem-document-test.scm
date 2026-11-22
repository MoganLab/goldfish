;; render-md-stem-document
;; 组织顶层块间距并连接行列表，得到多行 Markdown 文本。
;;
;; 语法
;; ----
;; (render-md-stem-document body)
;;
;; 参数
;; ----
;; body : list
;; 已校验文档体（(md-stem ...) 的 cdr，即顶层块列表）。
;;
;; 返回值
;; ----
;; string
;; 多行 Markdown 文本；顶层块之间以空行分隔，不含末尾换行；空文档为空字符串。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown document))

(check-set-mode! 'report-failed)

(check-true (procedure? render-md-stem-document))

;; 空文档
(check (render-md-stem-document '()) => "")

;; 单块
(check (render-md-stem-document '((p "正文"))) => "正文")
(check (render-md-stem-document '((h1 "标题"))) => "# 标题")

;; 相邻块之间以空行分隔
(check (render-md-stem-document '((p "a") (p "b"))) => "a\n\nb")
(check (render-md-stem-document '((h1 "t") (p "a") (thematic-break)))
  => "# t\n\na\n\n---"
) ;check

;; 非法节点沿块层校验报错
(check-catch 'md-stem-error (render-md-stem-document '((bad))))

(check-report)
