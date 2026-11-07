;; fmt-config-goldfish-version
;; TODO: 补充 fmt-config-goldfish-version 的简要描述。
;;
;; 语法
;; ----
;; (fmt-config-goldfish-version ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt config))

(check-set-mode! 'report-failed)

;; TODO: 补充 fmt-config-goldfish-version 的断言用例。
(check-true (procedure? fmt-config-goldfish-version))

(check-report)
