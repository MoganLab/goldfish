;; check-lang
;; TODO: 补充 check-lang 的简要描述。
;;
;; 语法
;; ----
;; (check-lang ...)
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

;; TODO: 补充 check-lang 的断言用例。
(check-true (procedure? check-lang))

(check-report)
