;; format-cpp-directory
;; TODO: 补充 format-cpp-directory 的简要描述。
;;
;; 语法
;; ----
;; (format-cpp-directory ...)
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

;; TODO: 补充 format-cpp-directory 的断言用例。
(check-true (procedure? format-cpp-directory))

(check-report)
