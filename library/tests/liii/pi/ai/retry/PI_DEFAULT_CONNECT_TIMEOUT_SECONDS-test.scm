;; PI_DEFAULT_CONNECT_TIMEOUT_SECONDS
;; 默认 HTTP 连接超时常量（秒），对齐上游 pi 的 undici 默认连接超时（10s）。
;;
;; 语法
;; ----
;; PI_DEFAULT_CONNECT_TIMEOUT_SECONDS
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; integer?
;; 连接超时秒数（10）。

(import (liii check) (liii pi ai retry))

(check-set-mode! 'report-failed)

(check PI_DEFAULT_CONNECT_TIMEOUT_SECONDS => 10)

(check-report)
