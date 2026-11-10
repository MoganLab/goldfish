;; fmt-jobs
;; TODO: 补充 fmt-jobs 的简要描述。
;;
;; 语法
;; ----
;; (fmt-jobs ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt parallel))

(check-set-mode! 'report-failed)

;; TODO: 补充 fmt-jobs 的断言用例。
(check-true (procedure? fmt-jobs))

(check-report)
