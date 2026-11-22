;; check-block-node
;; 校验单个块级节点的形状、必填字段与枚举值。
;;
;; 语法
;; ----
;; (check-block-node node)
;;
;; 参数
;; ----
;; node : any
;; 待校验的块节点。
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

(check-true (procedure? check-block-node))

;; 各类合法块节点
(check-true (check-block-node '(p "文本" (em "强调"))))
(check-true (check-block-node '(h3 "标题")))
(check-true (check-block-node '(h1)))
(check-true (check-block-node '(thematic-break)))
(check-true (check-block-node '(code-block "ruby" "def foo")))
(check-true (check-block-node '(math-block "E = mc^2")))
(check-true (check-block-node '(raw-block "html" "<div>")))

;; 未知块类型与形状错误
(check-catch 'md-stem-error (check-block-node '(unknown "x")))
(check-catch 'md-stem-error (check-block-node 42))
(check-catch 'md-stem-error (check-block-node "字符串"))

;; 字段数量错误
(check-catch 'md-stem-error (check-block-node '(thematic-break "多余")))
(check-catch 'md-stem-error (check-block-node '(code-block "info")))
(check-catch 'md-stem-error (check-block-node '(code-block "i" "c" "多余")))
(check-catch 'md-stem-error (check-block-node '(math-block "x" "多余")))

;; 字段类型错误
(check-catch 'md-stem-error (check-block-node '(code-block 123 "content")))
(check-catch 'md-stem-error (check-block-node '(math-block 42)))
(check-catch 'md-stem-error (check-block-node '(raw-block "html" 99)))

;; 段落行内内容含非法节点
(check-catch 'md-stem-error (check-block-node '(p (unknown-inline "x"))))

(check-report)
