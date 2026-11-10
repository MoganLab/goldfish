;; display-version
;; TODO: 补充 display-version 的简要描述。
;;
;; 语法
;; ----
;; (display-version ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldversion))

(check-set-mode! 'report-failed)

;; TODO: 补充 display-version 的断言用例。
(check-true (procedure? display-version))

(check-report)
