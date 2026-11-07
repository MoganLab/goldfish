;; load-fmt-config
;; TODO: 补充 load-fmt-config 的简要描述。
;;
;; 语法
;; ----
;; (load-fmt-config ...)
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

;; TODO: 补充 load-fmt-config 的断言用例。
(check-true (procedure? load-fmt-config))

(check-report)
