;; has-wildcard?
;; TODO: 补充 has-wildcard? 的简要描述。
;;
;; 语法
;; ----
;; (has-wildcard? ...)
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

;; TODO: 补充 has-wildcard? 的断言用例。
(check-true (procedure? has-wildcard?))

(check-report)
