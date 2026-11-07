;; library-query->display-name
;; TODO: 补充 library-query->display-name 的简要描述。
;;
;; 语法
;; ----
;; (library-query->display-name ...)
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

;; TODO: 补充 library-query->display-name 的断言用例。
(check-true (procedure? library-query->display-name))

(check-report)
