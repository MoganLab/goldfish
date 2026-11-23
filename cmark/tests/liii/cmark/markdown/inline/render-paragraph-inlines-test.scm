;; render-paragraph-inlines
;; 段落行内渲染：soft-break 渲染为换行、line-break 为两空格加换行，
;; 返回多行行列表；换行后的首个文本项按行首规则转义。
;;
;; 语法
;; ----
;; (render-paragraph-inlines items)
;;
;; 参数
;; ----
;; items : list
;; 字符串与行内节点混合的列表。
;;
;; 返回值
;; ----
;; list
;; 段落逻辑行字符串列表。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown inline))

(check-set-mode! 'report-failed)

(check-true (procedure? render-paragraph-inlines))

(check (render-paragraph-inlines '("单行")) => '("单行"))
(check (render-paragraph-inlines '("第一行" (soft-break) "第二行"))
  =>
  '("第一行" "第二行")
) ;check
(check (render-paragraph-inlines '("a" (line-break) "b")) => '("a  " "b"))
;; 换行后的首项文本按行首规则转义
(check (render-paragraph-inlines '("a" (soft-break) "#标题"))
  =>
  '("a" "\\#标题")
) ;check
(check (render-paragraph-inlines '("a" (soft-break) "- 列表"))
  =>
  '("a" "\\- 列表")
) ;check
;; 行内节点不受行首规则影响
(check (render-paragraph-inlines '("a" (soft-break) (em "b"))) => '("a" "*b*"))

;; ---- 行首转义不被空串首项击穿 ----
;; 段落首项为空串时，后续非空文本仍按行首规则转义，避免变成列表/标题
(check (render-paragraph-inlines '("" "- x")) => '("\\- x"))
(check (render-paragraph-inlines '("" "# x")) => '("\\# x"))
(check (render-paragraph-inlines '("" "1. x")) => '("1\\. x"))
;; 多个空串首项也不消耗行首状态
(check (render-paragraph-inlines '("" "" "- x")) => '("\\- x"))
;; 换行后空串首项同样不击穿
(check (render-paragraph-inlines '("a" (soft-break) "" "- x"))
  =>
  '("a" "\\- x")
) ;check

(check-report)
