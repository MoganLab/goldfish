;; key-error
;; 抛出键不存在错误。
;;
;; 语法
;; ----
;; (key-error . args)
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

(check-catch 'key-error (key-error))
(check-catch 'key-error (key-error "key 'foo not found"))

(check-report)
