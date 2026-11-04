;; value-error
;; 抛出数值或取值不合法错误。
;;
;; 语法
;; ----
;; (value-error . args)
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

(check-catch 'value-error (value-error))
(check-catch 'value-error (value-error "value out of domain"))

(check-report)
