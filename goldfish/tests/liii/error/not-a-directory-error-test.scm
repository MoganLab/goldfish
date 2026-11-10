;; not-a-directory-error
;; 抛出路径非目录错误。
;;
;; 语法
;; ----
;; (not-a-directory-error . args)
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

(check-catch 'not-a-directory-error (not-a-directory-error))
(check-catch 'not-a-directory-error (not-a-directory-error "not a dir"))

(check-report)
