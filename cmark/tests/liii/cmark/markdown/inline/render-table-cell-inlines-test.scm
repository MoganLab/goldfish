;; render-table-cell-inlines
;; 表格单元格渲染：文本额外转义管道符。
;;
;; 语法
;; ----
;; (render-table-cell-inlines items)
;;
;; 参数
;; ----
;; items : list
;; 字符串与行内节点混合的列表。
;;
;; 返回值
;; ----
;; string
;; 渲染得到的单元格 Markdown 文本。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown inline))

(check-set-mode! 'report-failed)

(check-true (procedure? render-table-cell-inlines))

(check (render-table-cell-inlines '()) => "")
(check (render-table-cell-inlines '("a|b")) => "a\\|b")
(check (render-table-cell-inlines '("a*b")) => "a\\*b")
(check (render-table-cell-inlines '((em "斜体"))) => "*斜体*")
(check (render-table-cell-inlines '((code "x|y"))) => "`x|y`")

(check-report)
