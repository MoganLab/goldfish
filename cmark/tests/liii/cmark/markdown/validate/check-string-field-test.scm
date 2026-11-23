;; check-string-field
;; 校验节点字段是字符串，否则抛出带节点类型的 md-stem-error。
;;
;; 语法
;; ----
;; (check-string-field node field value)
;;
;; 参数
;; ----
;; node : pair
;; 所在节点，用于错误信息。
;; field : string
;; 字段名描述。
;; value : any
;; 待校验的字段值。
;;
;; 返回值
;; ----
;; unspecified
;; 校验通过时无返回值。
;;
;; 错误处理
;; ----
;; value 不是字符串时抛出 md-stem-error。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown validate))

(check-set-mode! 'report-failed)

(check-true (procedure? check-string-field))

(check-true (check-string-field '(p "x") "内容" "文本"))
(check-true (check-string-field '(p "x") "内容" ""))
(check-catch 'md-stem-error (check-string-field '(p "x") "内容" 42))
(check-catch 'md-stem-error (check-string-field '(p "x") "内容" 'sym))

(check-report)
