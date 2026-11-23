;; check-inline-node
;; 校验单个行内节点的形状、必填字段与枚举值。
;;
;; 语法
;; ----
;; (check-inline-node node)
;;
;; 参数
;; ----
;; node : any
;; 待校验的行内节点。
;;
;; 返回值
;; ----
;; unspecified
;; 校验通过时无返回值。
;;
;; 错误处理
;; ----
;; 未知类型、字段数量或类型错误时抛出 md-stem-error。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown validate))

(check-set-mode! 'report-failed)

(check-true (procedure? check-inline-node))

;; 各类合法行内节点
(check-true (check-inline-node '(em "强调")))
(check-true (check-inline-node '(strong (em "嵌套"))))
(check-true (check-inline-node '(strikeout "删除")))
(check-true (check-inline-node '(code "gf test")))
(check-true (check-inline-node '(soft-break)))
(check-true (check-inline-node '(line-break)))
(check-true (check-inline-node '(link "url" "标题" "文本")))
(check-true (check-inline-node '(image "u.png" "" "替代")))
(check-true (check-inline-node '(math-inline "x^2")))
(check-true (check-inline-node '(raw-inline "html" "<br>")))
(check-true (check-inline-node '(emoji "smile")))

;; 未知类型与形状错误
(check-catch 'md-stem-error (check-inline-node '(unknown-inline "x")))
(check-catch 'md-stem-error (check-inline-node 42))

;; 字段数量与类型错误
(check-catch 'md-stem-error (check-inline-node '(code)))
(check-catch 'md-stem-error (check-inline-node '(code 42)))
(check-catch 'md-stem-error (check-inline-node '(code "x" "多余")))
(check-catch 'md-stem-error (check-inline-node '(link "url")))
(check-catch 'md-stem-error (check-inline-node '(link 1 2 "文本")))
(check-catch 'md-stem-error (check-inline-node '(raw-inline "html")))
(check-catch 'md-stem-error (check-inline-node '(raw-inline "html" 42)))
(check-catch 'md-stem-error (check-inline-node '(emoji :not-string)))
(check-catch 'md-stem-error (check-inline-node '(soft-break "多余")))

;; 嵌套行内含非法节点
(check-catch 'md-stem-error (check-inline-node '(em (unknown-inline "x"))))

(check-report)
