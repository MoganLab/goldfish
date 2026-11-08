;; timeout-error
;; 抛出超时错误。
;;
;; 语法
;; ----
;; (timeout-error . args)
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

(check-catch 'timeout-error (timeout-error))
(check-catch 'timeout-error (timeout-error "timed out after 5s"))

(check-report)
