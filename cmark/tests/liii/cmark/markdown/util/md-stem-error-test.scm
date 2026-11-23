;; md-stem-error
;; 产生带节点类型的 md-stem 反向转换统一错误。
;;
;; 语法
;; ----
;; (md-stem-error node-type message ...)
;;
;; 参数
;; ----
;; node-type : symbol
;; 出错的节点类型符号。
;; message : any
;; 附加的错误描述参数。
;;
;; 返回值
;; ----
;; 不会正常返回；总是抛出 (error 'md-stem-error ...) 异常。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown util))

(check-set-mode! 'report-failed)

(check-true (procedure? md-stem-error))

(check-catch 'md-stem-error (md-stem-error 'p "段落错误"))
(check-catch 'md-stem-error (md-stem-error 'table "表格错误" '额外参数))

(check-report)
