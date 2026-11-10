;; scheme-check-files
;; TODO: 补充 scheme-check-files 的简要描述。
;;
;; 语法
;; ----
;; (scheme-check-files ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt scheme))

(check-set-mode! 'report-failed)

;; TODO: 补充 scheme-check-files 的断言用例。
(check-true (procedure? scheme-check-files))

(check-report)
