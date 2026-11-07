;; display-library-function-suggestions
;; TODO: 补充 display-library-function-suggestions 的简要描述。
;;
;; 语法
;; ----
;; (display-library-function-suggestions ...)
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

;; TODO: 补充 display-library-function-suggestions 的断言用例。
(check-true (procedure? display-library-function-suggestions))

(check-report)
