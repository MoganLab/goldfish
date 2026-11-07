;; golddoc-command-name
;; TODO: 补充 golddoc-command-name 的简要描述。
;;
;; 语法
;; ----
;; (golddoc-command-name ...)
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

;; TODO: 补充 golddoc-command-name 的断言用例。
(check-true (procedure? golddoc-command-name))

(check-report)
