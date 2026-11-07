;; all-registered-extensions
;; TODO: 补充 all-registered-extensions 的简要描述。
;;
;; 语法
;; ----
;; (all-registered-extensions ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt changed))

(check-set-mode! 'report-failed)

;; TODO: 补充 all-registered-extensions 的断言用例。
(check-true (procedure? all-registered-extensions))

(check-report)
