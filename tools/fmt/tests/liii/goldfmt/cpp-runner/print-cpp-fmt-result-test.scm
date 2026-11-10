;; print-cpp-fmt-result
;; TODO: 补充 print-cpp-fmt-result 的简要描述。
;;
;; 语法
;; ----
;; (print-cpp-fmt-result ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt cpp-runner))

(check-set-mode! 'report-failed)

;; TODO: 补充 print-cpp-fmt-result 的断言用例。
(check-true (procedure? print-cpp-fmt-result))

(check-report)
