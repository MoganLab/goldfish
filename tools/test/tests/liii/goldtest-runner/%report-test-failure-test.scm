;; %report-test-failure
;; TODO: 补充 %report-test-failure 的简要描述。
;;
;; 语法
;; ----
;; (%report-test-failure ...)
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

;; TODO: 补充 %report-test-failure 的断言用例。
(check-true (procedure? %report-test-failure))

(check-report)
