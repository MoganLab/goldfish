;; stem-fmt-one
;; TODO: 补充 stem-fmt-one 的简要描述。
;;
;; 语法
;; ----
;; (stem-fmt-one ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt stem-fmt))

(check-set-mode! 'report-failed)

;; TODO: 补充 stem-fmt-one 的断言用例。
(check-true (procedure? stem-fmt-one))

(check-report)
