;; file-error
;; 抛出通用文件操作错误。
;;
;; 语法
;; ----
;; (file-error . args)
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

(check-catch 'file-error (file-error))
(check-catch 'file-error (file-error "file operation failed"))

(check-report)
