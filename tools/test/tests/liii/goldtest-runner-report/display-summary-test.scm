;; display-summary
;; TODO: 补充 display-summary 的简要描述。
;;
;; 语法
;; ----
;; (display-summary ...)
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

;; TODO: 补充 display-summary 的断言用例。
(check-true (procedure? display-summary))

(check-report)
