;; pi-bash-clean-output
;; TODO: 补充 pi-bash-clean-output 的简要描述。
;;
;; 语法
;; ----
;; (pi-bash-clean-output ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii pi code core tools bash))

(check-set-mode! 'report-failed)

;; TODO: 补充 pi-bash-clean-output 的断言用例。
(check-true (procedure? pi-bash-clean-output))

(check-report)
