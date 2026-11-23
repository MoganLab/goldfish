;; escape-image-alt-plain-text
;; 图片替代文本的纯文本片段转义：转义 [ ] 等会提前终止 alt 的字符。
;;
;; 语法
;; ----
;; (escape-image-alt-plain-text str)
;;
;; 参数
;; ----
;; str : string
;; 替代文本片段。
;;
;; 返回值
;; ----
;; string
;; 转义后的文本。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown text))

(check-set-mode! 'report-failed)

(check-true (procedure? escape-image-alt-plain-text))

(check (escape-image-alt-plain-text "") => "")
(check (escape-image-alt-plain-text "替代文本") => "替代文本")
(check (escape-image-alt-plain-text "a[b]c") => "a\\[b\\]c")
(check (escape-image-alt-plain-text "a*b") => "a\\*b")

(check-report)
