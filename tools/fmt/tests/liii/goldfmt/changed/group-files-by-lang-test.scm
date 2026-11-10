;; group-files-by-lang
;; TODO: 补充 group-files-by-lang 的简要描述。
;;
;; 语法
;; ----
;; (group-files-by-lang ...)
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

;; TODO: 补充 group-files-by-lang 的断言用例。
(check-true (procedure? group-files-by-lang))

(check-report)
