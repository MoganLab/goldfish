;; display-build-json-hint
;; TODO: 补充 display-build-json-hint 的简要描述。
;;
;; 语法
;; ----
;; (display-build-json-hint ...)
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

;; TODO: 补充 display-build-json-hint 的断言用例。
(check-true (procedure? display-build-json-hint))

(check-report)
