;; format-changed-since
;; TODO: 补充 format-changed-since 的简要描述。
;;
;; 语法
;; ----
;; (format-changed-since ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt changed))

(check-set-mode! 'report-failed)

;; TODO: 补充 format-changed-since 的断言用例。
(check-true (procedure? format-changed-since))

(check-report)
