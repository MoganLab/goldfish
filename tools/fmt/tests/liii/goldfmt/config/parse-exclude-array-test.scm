;; parse-exclude-array
;; TODO: 补充 parse-exclude-array 的简要描述。
;;
;; 语法
;; ----
;; (parse-exclude-array ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt config))

(check-set-mode! 'report-failed)

;; TODO: 补充 parse-exclude-array 的断言用例。
(check-true (procedure? parse-exclude-array))

(check-report)
