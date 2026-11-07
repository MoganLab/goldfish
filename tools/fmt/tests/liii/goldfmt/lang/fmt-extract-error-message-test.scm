;; fmt-extract-error-message
;; TODO: 补充 fmt-extract-error-message 的简要描述。
;;
;; 语法
;; ----
;; (fmt-extract-error-message ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt lang))

(check-set-mode! 'report-failed)

;; TODO: 补充 fmt-extract-error-message 的断言用例。
(check-true (procedure? fmt-extract-error-message))

(check-report)
