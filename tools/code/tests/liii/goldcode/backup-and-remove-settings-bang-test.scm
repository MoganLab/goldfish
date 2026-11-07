;; backup-and-remove-settings!
;; TODO: 补充 backup-and-remove-settings! 的简要描述。
;;
;; 语法
;; ----
;; (backup-and-remove-settings! ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldcode))

(check-set-mode! 'report-failed)

;; TODO: 补充 backup-and-remove-settings! 的断言用例。
(check-true (procedure? backup-and-remove-settings!))

(check-report)
