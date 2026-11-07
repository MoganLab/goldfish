;; check-goldfish-version
;; TODO: 补充 check-goldfish-version 的简要描述。
;;
;; 语法
;; ----
;; (check-goldfish-version ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt repo))

(check-set-mode! 'report-failed)

;; TODO: 补充 check-goldfish-version 的断言用例。
(check-true (procedure? check-goldfish-version))

(check-report)
