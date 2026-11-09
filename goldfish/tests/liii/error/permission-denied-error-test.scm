;; permission-denied-error
;; 抛出权限不足错误。
;;
;; 语法
;; ----
;; (permission-denied-error . args)
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

(check-catch 'permission-denied-error (permission-denied-error))
(check-catch 'permission-denied-error (permission-denied-error "access denied"))

(check-report)
