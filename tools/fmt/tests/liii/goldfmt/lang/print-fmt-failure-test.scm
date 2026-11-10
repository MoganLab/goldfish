;; print-fmt-failure
;; TODO: 补充 print-fmt-failure 的简要描述。
;;
;; 语法
;; ----
;; (print-fmt-failure ...)
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

;; TODO: 补充 print-fmt-failure 的断言用例。
(check-true (procedure? print-fmt-failure))

(check-report)
