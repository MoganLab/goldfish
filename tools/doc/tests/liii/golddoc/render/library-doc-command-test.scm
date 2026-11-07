;; library-doc-command
;; TODO: 补充 library-doc-command 的简要描述。
;;
;; 语法
;; ----
;; (library-doc-command ...)
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

;; TODO: 补充 library-doc-command 的断言用例。
(check-true (procedure? library-doc-command))

(check-report)
