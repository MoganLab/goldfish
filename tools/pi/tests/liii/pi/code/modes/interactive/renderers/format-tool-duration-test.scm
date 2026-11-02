;; format-tool-duration
;; format-tool-duration 的相关操作与逻辑实现。
;;
;; 语法
;; ----
;; (format-tool-duration ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; any
;; 视具体调用结果而定。

(import (scheme base) (liii check) (liii pi code modes interactive renderers))

(check-set-mode! 'report-failed)

;; 不足一分钟保留一位小数，其余与上游 formatToolDuration 一致
(check (format-tool-duration 4200) => "4.2s")
(check (format-tool-duration 61000) => "1m 1s")
(check (format-tool-duration 3661000) => "1h 1m 1s")

(check-report)
