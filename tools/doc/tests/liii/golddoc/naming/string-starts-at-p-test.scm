;; string-starts-at?
;; TODO: 补充 string-starts-at? 的简要描述。
;;
;; 语法
;; ----
;; (string-starts-at? ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii golddoc naming))

(check-set-mode! 'report-failed)

;; TODO: 补充 string-starts-at? 的断言用例。
(check-true (procedure? string-starts-at?))

(check-report)
