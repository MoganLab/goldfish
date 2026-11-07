;; parse-excludes
;; TODO: 补充 parse-excludes 的简要描述。
;;
;; 语法
;; ----
;; (parse-excludes ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt args))

(check-set-mode! 'report-failed)

;; TODO: 补充 parse-excludes 的断言用例。
(check-true (procedure? parse-excludes))

(check-report)
