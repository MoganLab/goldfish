;; type-error
;; 抛出类型错误。
;;
;; 语法
;; ----
;; (type-error . args)
;;
;; 参数
;; ----
;; args : any
;; 错误描述信息。

;; 返回值
;; ----
;; 不返回
;; 抛出对应错误，不返回正常值。

(import (liii check)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

(check-catch 'type-error (type-error))
(check-catch 'type-error (type-error "expected integer"))
(check-catch 'type-error (type-error "expected integer" "got string"))

(check-report)
