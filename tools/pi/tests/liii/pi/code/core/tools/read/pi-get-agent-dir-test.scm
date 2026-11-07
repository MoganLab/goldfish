;; pi-get-agent-dir
;; TODO: 补充 pi-get-agent-dir 的简要描述。
;;
;; 语法
;; ----
;; (pi-get-agent-dir ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii pi code core tools read))

(check-set-mode! 'report-failed)

;; TODO: 补充 pi-get-agent-dir 的断言用例。
(check-true (procedure? pi-get-agent-dir))

(check-report)
