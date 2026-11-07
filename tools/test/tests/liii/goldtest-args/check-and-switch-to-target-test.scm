;; check-and-switch-to-target
;; TODO: 补充 check-and-switch-to-target 的简要描述。
;;
;; 语法
;; ----
;; (check-and-switch-to-target ...)
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

;; TODO: 补充 check-and-switch-to-target 的断言用例。
(check-true (procedure? check-and-switch-to-target))

(check-report)
