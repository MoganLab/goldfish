;; ensure-scheme-handler!
;; TODO: 补充 ensure-scheme-handler! 的简要描述。
;;
;; 语法
;; ----
;; (ensure-scheme-handler! ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt dispatch))

(check-set-mode! 'report-failed)

;; TODO: 补充 ensure-scheme-handler! 的断言用例。
(check-true (procedure? ensure-scheme-handler!))

(check-report)
