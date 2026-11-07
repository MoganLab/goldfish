;; PI_MAX_TIMEOUT_SECONDS
;; TODO: 补充 PI_MAX_TIMEOUT_SECONDS 的简要描述。
;;
;; 语法
;; ----
;; (PI_MAX_TIMEOUT_SECONDS ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii pi code core tools bash))

(check-set-mode! 'report-failed)

;; 常量值断言
(check PI_MAX_TIMEOUT_SECONDS => 2147483.647)
(check-true (real? PI_MAX_TIMEOUT_SECONDS))

(check-report)
