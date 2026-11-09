;; index-error
;; 抛出索引越界错误。
;;
;; 语法
;; ----
;; (index-error . args)
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

(check-catch 'index-error (index-error))
(check-catch 'index-error (index-error "index 5 out of range"))

(check-report)
