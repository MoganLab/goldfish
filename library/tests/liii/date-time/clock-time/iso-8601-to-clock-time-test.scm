(import (liii check)
        (liii date-time clock-time))

(check-set-mode! 'report-failed)

;; iso-8601->clock-time
;; 将符合 ISO-8601 格式的时间字符串解析为 clock-time 对象。
;;
;; 语法
;; ----
;; (iso-8601->clock-time str)
;;
;; 参数
;; ----
;; str : string? 符合 ISO-8601 时间格式的字符串，如 "23:01:02" 或 "03:04:05.1"。
;;
;; 返回值
;; ----
;; clock-time? 对应的时钟时间对象。
;;
;; 示例
;; ----
;; (iso-8601->clock-time "23:01:02") => (make-clock-time 23 1 2)
;;
;; 错误处理
;; --------
;; type-error 当参数不是字符串或格式非法时抛出错误。

(check-true (clock-time=? (iso-8601->clock-time "23:01:02") (make-clock-time 23 1 2)))
(check-true (clock-time=? (iso-8601->clock-time "00:00:00") (make-clock-time 0 0 0)))
(check-true (clock-time=? (iso-8601->clock-time "03:04:05.1") (make-clock-time 3 4 (+ 5 1/10))))

(check-catch 'type-error (iso-8601->clock-time 230102))
(check-catch 'type-error (iso-8601->clock-time "invalid"))
(check-catch 'type-error (iso-8601->clock-time "24:00:00"))
(check-catch 'type-error (iso-8601->clock-time "12:60:00"))

(check-report)
