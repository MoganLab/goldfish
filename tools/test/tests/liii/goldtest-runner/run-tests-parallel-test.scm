;; run-tests-parallel
;; TODO: 补充 run-tests-parallel 的简要描述。
;;
;; 语法
;; ----
;; (run-tests-parallel ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldtest-runner))

(check-set-mode! 'report-failed)

;; TODO: 补充 run-tests-parallel 的断言用例。
(check-true (procedure? run-tests-parallel))

(check-report)
