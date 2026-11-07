;; color
;; TODO: 补充 color 的简要描述。
;;
;; 语法
;; ----
;; (color ...)
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

;; TODO: 补充 color 的断言用例。
(check-true (procedure? color))

(check-report)
