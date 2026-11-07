;; format-cpp-file
;; TODO: 补充 format-cpp-file 的简要描述。
;;
;; 语法
;; ----
;; (format-cpp-file ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt cpp))

(check-set-mode! 'report-failed)

;; TODO: 补充 format-cpp-file 的断言用例。
(check-true (procedure? format-cpp-file))

(check-report)
