;; check-files-or-serial
;; TODO: 补充 check-files-or-serial 的简要描述。
;;
;; 语法
;; ----
;; (check-files-or-serial ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt repo))

(check-set-mode! 'report-failed)

;; TODO: 补充 check-files-or-serial 的断言用例。
(check-true (procedure? check-files-or-serial))

(check-report)
