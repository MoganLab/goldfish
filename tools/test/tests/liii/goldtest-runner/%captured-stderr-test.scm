;; %captured-stderr
;; TODO: 补充 %captured-stderr 的简要描述。
;;
;; 语法
;; ----
;; (%captured-stderr ...)
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

;; TODO: 补充 %captured-stderr 的断言用例。
(check-true (procedure? %captured-stderr))

(check-report)
