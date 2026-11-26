;; zlib-error
;; 抛出 zlib 压缩/解压运行时错误。
;;
;; 语法
;; ----
;; (zlib-error . args)
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

(check-catch 'zlib-error (zlib-error))
(check-catch 'zlib-error (zlib-error "gzip-decompress: incorrect header check (code -3)"))

(check-report)
