;; suffix-match?
;; TODO: 补充 suffix-match? 的简要描述。
;;
;; 语法
;; ----
;; (suffix-match? ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt path))

(check-set-mode! 'report-failed)

;; TODO: 补充 suffix-match? 的断言用例。
(check-true (procedure? suffix-match?))

(check-report)
