;; file-exists-error
;; 抛出文件已存在错误。
;;
;; 语法
;; ----
;; (file-exists-error . args)
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

(check-catch 'file-exists-error (file-exists-error))
(check-catch 'file-exists-error (file-exists-error "already exists"))

(check-report)
