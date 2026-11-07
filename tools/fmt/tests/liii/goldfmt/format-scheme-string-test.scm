;; format-scheme-string
;; TODO: 补充 format-scheme-string 的简要描述。
;;
;; 语法
;; ----
;; (format-scheme-string ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt))

(check-set-mode! 'report-failed)

;; TODO: 补充 format-scheme-string 的断言用例。
(check-true (procedure? format-scheme-string))

(check-report)
