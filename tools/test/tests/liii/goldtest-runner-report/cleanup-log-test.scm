;; cleanup-log
;; TODO: 补充 cleanup-log 的简要描述。
;;
;; 语法
;; ----
;; (cleanup-log ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldtest-runner-report))

(check-set-mode! 'report-failed)

;; TODO: 补充 cleanup-log 的断言用例。
(check-true (procedure? cleanup-log))

(check-report)
