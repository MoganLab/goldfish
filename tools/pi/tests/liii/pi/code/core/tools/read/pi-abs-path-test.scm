;; pi-abs-path
;; TODO: 补充 pi-abs-path 的简要描述。
;;
;; 语法
;; ----
;; (pi-abs-path ...)
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

;; TODO: 补充 pi-abs-path 的断言用例。
(check-true (procedure? pi-abs-path))

(check-report)
