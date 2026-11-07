;; shell-double-quote
;; TODO: 补充 shell-double-quote 的简要描述。
;;
;; 语法
;; ----
;; (shell-double-quote ...)
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

;; TODO: 补充 shell-double-quote 的断言用例。
(check-true (procedure? shell-double-quote))

(check-report)
