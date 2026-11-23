;; render-inlines-at-line-start
;; 行首位置渲染行内内容：首项纯文本按块首规则转义。
;;
;; 语法
;; ----
;; (render-inlines-at-line-start items)
;;
;; 参数
;; ----
;; items : list
;; 字符串与行内节点混合的列表。
;;
;; 返回值
;; ----
;; string
;; 渲染得到的 Markdown 行内文本。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown inline))

(check-set-mode! 'report-failed)

(check-true (procedure? render-inlines-at-line-start))

;; 行首特殊文本被转义
(check (render-inlines-at-line-start '("#不是标题")) => "\\#不是标题")
(check (render-inlines-at-line-start '("- 列表？")) => "\\- 列表？")
(check (render-inlines-at-line-start '("1. 编号？")) => "1\\. 编号？")
(check (render-inlines-at-line-start '("> 引用？")) => "\\> 引用？")
(check (render-inlines-at-line-start '("* 星号")) => "\\* 星号")

;; 行首是节点时不受影响
(check (render-inlines-at-line-start '((em "斜体"))) => "*斜体*")

;; 非首项不受行首规则影响（行中 # 无语法含义，不转义）
(check (render-inlines-at-line-start '("a" "#不是标题"))
  =>
  "a#不是标题"
) ;check

;; ---- 行首转义不被空串首项击穿 ----
;; 空串首项不消耗行首状态，后续非空文本仍按行首规则转义
(check (render-inlines-at-line-start '("" "- x")) => "\\- x")
(check (render-inlines-at-line-start '("" "# x")) => "\\# x")
(check (render-inlines-at-line-start '("" "1. x")) => "1\\. x")
(check (render-inlines-at-line-start '("" "> x")) => "\\> x")
;; 多个空串首项也不消耗行首状态
(check (render-inlines-at-line-start '("" "" "- x")) => "\\- x")
;; 空串后跟非行首特殊文本保持不变
(check (render-inlines-at-line-start '("" "普通")) => "普通")

(check-report)
