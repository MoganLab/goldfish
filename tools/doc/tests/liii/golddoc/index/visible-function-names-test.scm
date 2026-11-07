;; visible-function-names
;; TODO: 补充 visible-function-names 的简要描述。
;;
;; 语法
;; ----
;; (visible-function-names ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii golddoc index))

(check-set-mode! 'report-failed)

;; TODO: 补充 visible-function-names 的断言用例。
(check-true (procedure? visible-function-names))

(check-report)
