;; parse-test-jobs
;; TODO: 补充 parse-test-jobs 的简要描述。
;;
;; 语法
;; ----
;; (parse-test-jobs ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldtest-args))

(check-set-mode! 'report-failed)

;; TODO: 补充 parse-test-jobs 的断言用例。
(check-true (procedure? parse-test-jobs))

(check-report)
