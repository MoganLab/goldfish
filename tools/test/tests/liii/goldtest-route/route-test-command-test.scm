;; route-test-command
;; TODO: 补充 route-test-command 的简要描述。
;;
;; 语法
;; ----
;; (route-test-command ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldtest-route))

(check-set-mode! 'report-failed)

;; TODO: 补充 route-test-command 的断言用例。
(check-true (procedure? route-test-command))

(check-report)
