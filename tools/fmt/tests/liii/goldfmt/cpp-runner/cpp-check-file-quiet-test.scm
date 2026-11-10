;; cpp-check-file-quiet
;; TODO: 补充 cpp-check-file-quiet 的简要描述。
;;
;; 语法
;; ----
;; (cpp-check-file-quiet ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt cpp-runner))

(check-set-mode! 'report-failed)

;; TODO: 补充 cpp-check-file-quiet 的断言用例。
(check-true (procedure? cpp-check-file-quiet))

(check-report)
