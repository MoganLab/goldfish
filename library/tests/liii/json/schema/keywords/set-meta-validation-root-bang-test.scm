;; set-meta-validation-root!
;; 设置当前的元校验根资源。
;;
;; 语法
;; ----
;; (set-meta-validation-root! value)
;;
;; 参数
;; ----
;; value : any?
;; 要设置的元校验根资源，#f 表示清除。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值没有意义。
;;
;; 注意
;; ----
;; 元校验根资源是模块级的全局状态，
;; 用于在 check-schema 过程中把 $dynamicRef 解析到元 Schema。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema keywords) (liii json schema model))

(check-set-mode! 'report-failed)

(define meta-root '(("type" . "object")))

;; 设置后可以读到。

(set-meta-validation-root! meta-root)
(check (meta-validation-root) => meta-root)

;; 再次设置会覆盖旧值。

(set-meta-validation-root! '(("type" . "array")))
(check (meta-validation-root) => '(("type" . "array")))

;; 传入 #f 清除设置。

(set-meta-validation-root! #f)
(check-false (meta-validation-root))

(check-report)
