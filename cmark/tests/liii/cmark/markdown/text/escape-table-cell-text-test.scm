;; escape-table-cell-text
;; 表格单元格文本转义：在普通行内转义基础上额外转义管道符。
;;
;; 语法
;; ----
;; (escape-table-cell-text str)
;;
;; 参数
;; ----
;; str : string
;; 单元格原始纯文本。
;;
;; 返回值
;; ----
;; string
;; 转义后的文本。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown text))

(check-set-mode! 'report-failed)

(check-true (procedure? escape-table-cell-text))

(check (escape-table-cell-text "") => "")
(check (escape-table-cell-text "普通") => "普通")
(check (escape-table-cell-text "a|b") => "a\\|b")
(check (escape-table-cell-text "|a|") => "\\|a\\|")
(check (escape-table-cell-text "a*b") => "a\\*b")

(check-report)
