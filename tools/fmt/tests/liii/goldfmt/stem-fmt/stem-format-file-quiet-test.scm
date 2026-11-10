;; stem-format-file-quiet
;; TODO: 补充 stem-format-file-quiet 的简要描述。
;;
;; 语法
;; ----
;; (stem-format-file-quiet ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt stem-fmt))

(check-set-mode! 'report-failed)

;; TODO: 补充 stem-format-file-quiet 的断言用例。
(check-true (procedure? stem-format-file-quiet))

(check-report)
