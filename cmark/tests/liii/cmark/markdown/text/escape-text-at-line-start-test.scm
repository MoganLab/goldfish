;; escape-text-at-line-start
;; 块首文本转义：在普通转义基础上额外防止 # 标题、- + * 列表、> 引用、
;; 数字. / 数字) 有序列表标记被重新识别。
;;
;; 语法
;; ----
;; (escape-text-at-line-start str)
;;
;; 参数
;; ----
;; str : string
;; 行首原始纯文本。
;;
;; 返回值
;; ----
;; string
;; 转义后的文本。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown text))

(check-set-mode! 'report-failed)

(check-true (procedure? escape-text-at-line-start))

(check (escape-text-at-line-start "") => "")
(check (escape-text-at-line-start "普通文本") => "普通文本")
(check (escape-text-at-line-start "#不是标题") => "\\#不是标题")
(check (escape-text-at-line-start "- 列表？") => "\\- 列表？")
(check (escape-text-at-line-start "+ 加号") => "\\+ 加号")
(check (escape-text-at-line-start "* 星号") => "\\* 星号")
(check (escape-text-at-line-start "> 引用？") => "\\> 引用？")
(check (escape-text-at-line-start "1. 编号？") => "1\\. 编号？")
(check (escape-text-at-line-start "12) 括号") => "12\\) 括号")
;; 普通数字开头非列表
(check (escape-text-at-line-start "42 是答案") => "42 是答案")
;; 行首 a*b 等普通转义
(check (escape-text-at-line-start "a*b") => "a\\*b")

(check-report)
