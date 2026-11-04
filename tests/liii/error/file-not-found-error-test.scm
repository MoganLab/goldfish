;; file-not-found-error
;; 抛出文件未找到错误。
;;
;; 语法
;; ----
;; (file-not-found-error . args)
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

(check-catch 'file-not-found-error (file-not-found-error))
(check-catch 'file-not-found-error (file-not-found-error "missing.txt"))

(check-report)
