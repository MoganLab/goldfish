;; escape-text
;; 普通行内文本转义：backslash-escape 所有 Markdown 标记字符。
;;
;; 语法
;; ----
;; (escape-text str)
;;
;; 参数
;; ----
;; str : string
;; 原始纯文本。
;;
;; 返回值
;; ----
;; string
;; 转义后的文本；标记字符 \\ ` * _ [ ] < > 前插入反斜杠。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown text))

(check-set-mode! 'report-failed)

(check-true (procedure? escape-text))

(check (escape-text "") => "")
(check (escape-text "普通文本") => "普通文本")
(check (escape-text "a*b") => "a\\*b")
(check (escape-text "a_b") => "a\\_b")
(check (escape-text "反引号`代码`") => "反引号\\`代码\\`")
(check (escape-text "文本[方括号]end") => "文本\\[方括号\\]end")
(check (escape-text "a<b>c") => "a\\<b\\>c")
(check (escape-text "反斜杠\\") => "反斜杠\\\\")

;; 项目扩展语法字符：$ 数学、~~ 删除线
(check (escape-text "价格 5$ 和 6$") => "价格 5\\$ 和 6\\$")
(check (escape-text "~~删除~~") => "\\~\\~删除\\~\\~")
(check (escape-text ":fire:") => "\\:fire:")
(check (escape-text "看 https://example.com 链接") => "看 https\\://example.com 链接")
;; 普通冒号不转义（时间、比例）
(check (escape-text "时间 12:30") => "时间 12:30")
(check (escape-text "a:b") => "a:b")

(check-report)
