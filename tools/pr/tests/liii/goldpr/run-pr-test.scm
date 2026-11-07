;; run-pr
;; TODO: 补充 run-pr 的简要描述。
;;
;; 语法
;; ----
;; (run-pr ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldpr))

(check-set-mode! 'report-failed)

;; TODO: 补充 run-pr 的断言用例。
(check-true (procedure? run-pr))

(check-report)
