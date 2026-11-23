;; render-image-alt-items
;; 渲染图片替代文本项：文本片段按 alt 规则转义，math/emoji 保留语义。
;;
;; 语法
;; ----
;; (render-image-alt-items items)
;;
;; 参数
;; ----
;; items : list
;; 字符串、math-inline 或 emoji 节点的列表。
;;
;; 返回值
;; ----
;; string
;; 渲染得到的替代文本。
;;
;; 错误处理
;; ----
;; 出现其他节点类型时抛出 md-stem-error。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown inline))

(check-set-mode! 'report-failed)

(check-true (procedure? render-image-alt-items))

(check (render-image-alt-items '()) => "")
(check (render-image-alt-items '("")) => "")
(check (render-image-alt-items '("替代文本")) => "替代文本")
(check (render-image-alt-items '("a[b]c")) => "a\\[b\\]c")
(check (render-image-alt-items '("图 " (math-inline "x"))) => "图 $x$")
(check (render-image-alt-items '((emoji "smile"))) => ":smile:")
(check (render-image-alt-items '("前" (emoji "fire") "后")) => "前:fire:后")

;; 其他节点类型不允许出现在 alt
(check-catch 'md-stem-error (render-image-alt-items '((em "斜体"))))

(check-report)
