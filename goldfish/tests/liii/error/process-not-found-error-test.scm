;; process-not-found-error
;; 抛出进程未找到错误。
;;
;; 语法
;; ----
;; (process-not-found-error . args)
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

(check-catch 'process-not-found-error (process-not-found-error))
(check-catch 'process-not-found-error (process-not-found-error "pid 1234 not found"))

(check-report)
