;; config-exists?
;; TODO: 补充 config-exists? 的简要描述。
;;
;; 语法
;; ----
;; (config-exists? ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt config))

(check-set-mode! 'report-failed)

;; TODO: 补充 config-exists? 的断言用例。
(check-true (procedure? config-exists?))

(check-report)
