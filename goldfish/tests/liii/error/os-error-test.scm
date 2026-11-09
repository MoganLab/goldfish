;; os-error
;; 抛出操作系统级错误。
;;
;; 语法
;; ----
;; (os-error . args)
;;
;; 参数
;; ----
;; args : any
;; 错误描述信息或附加数据。

;; 返回值
;; ----
;; 不返回
;; 抛出对应错误，不返回正常值。

(import (liii check)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

(check-catch 'os-error (os-error))
(check-catch 'os-error (os-error "system failed"))

(check-report)
