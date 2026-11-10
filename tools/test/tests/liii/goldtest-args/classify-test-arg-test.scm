;; classify-test-arg
;; TODO: 补充 classify-test-arg 的简要描述。
;;
;; 语法
;; ----
;; (classify-test-arg ...)
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

;; TODO: 补充 classify-test-arg 的断言用例。
(check-true (procedure? classify-test-arg))

(check-report)
