;; format-markdown-string
;; 通过 Markdown -> md-stem -> Markdown 转换格式化文本，表格按显示宽度对齐。
;;
;; 语法
;; ----
;; (format-markdown-string source)
;;
;; 参数
;; ----
;; source : string? Markdown 源码。
;;
;; 返回值
;; ------
;; string? 规范化后的 Markdown 源码。

(import (scheme base) (liii check) (liii goldfmt markdown))

(check-set-mode! 'report-failed)

(define unformatted "| Name | Description |\n|:--|--:|\n| a | b |\n")

(define formatted
  "| Name | Description |\n| :--- | ----------: |\n| a    | b           |\n"
) ;define

(check (format-markdown-string unformatted) => formatted)
;; 格式化必须幂等。
(check (format-markdown-string formatted) => formatted)

;; string-length 是字节长度，中文列宽仍应由 cmark 的终端宽度计算对齐。
(check (format-markdown-string "| 名称 | 值 |\n|:---|---:|\n| 甲 | 1 |\n")
  =>
  "| 名称 | 值  |\n| :--- | --: |\n| 甲   | 1   |\n"
) ;check

;; 支持表格单元格中的行内节点。
(check (format-markdown-string "| A | B |\n|---|---|\n| **x** | [y](u) |\n")
  =>
  "| A     | B      |\n| ----- | ------ |\n| **x** | [y](u) |\n"
) ;check

(check (format-markdown-string "") => "")
(check-true (procedure? format-markdown-string))

(check-report)
