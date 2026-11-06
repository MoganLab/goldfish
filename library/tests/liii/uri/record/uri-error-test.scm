;; uri-error
;; 以 uri-error 条件类型抛出 URI 相关错误。
;;
;; 语法
;; ----
;; (uri-error arg ...)
;;
;; 参数
;; ----
;; arg : any ...
;;   传递给底层 error 的错误消息及附属值。
;;
;; 返回值
;; ----
;; 不返回
;;   总是抛出 uri-error 错误条件。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(check-catch 'uri-error (uri-error "invalid URI"))
(check-catch 'uri-error (uri-error "invalid component" "path"))

(check-report)
