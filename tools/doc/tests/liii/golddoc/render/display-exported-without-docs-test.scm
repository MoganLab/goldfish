;; display-exported-without-docs
;; TODO: 补充 display-exported-without-docs 的简要描述。
;;
;; 语法
;; ----
;; (display-exported-without-docs ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii golddoc render))

(check-set-mode! 'report-failed)

;; TODO: 补充 display-exported-without-docs 的断言用例。
(check-true (procedure? display-exported-without-docs))

(check-report)
