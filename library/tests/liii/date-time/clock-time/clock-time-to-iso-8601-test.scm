(import (liii check)
        (liii date-time clock-time))

(check-set-mode! 'report-failed)

;; clock-time->iso-8601
;; 将时钟时间对象转换为 ISO-8601 格式的时间字符串（如 "HH:MM:SS" 或 "HH:MM:SS.sss"）。
;;
;; 语法
;; ----
;; (clock-time->iso-8601 ct)
;;
;; 参数
;; ----
;; ct : clock-time? 时钟时间对象。
;;
;; 返回值
;; ----
;; string? ISO-8601 时间字符串。
;;
;; 示例
;; ----
;; (clock-time->iso-8601 (make-clock-time 23 1 2)) => "23:01:02"
;; (clock-time->iso-8601 (make-clock-time 1 2 (+ 3 1/10))) => "01:02:03.1"
;;
;; 错误处理
;; --------
;; type-error 当参数不是 clock-time 对象时抛出错误。

(check (clock-time->iso-8601 (make-clock-time 23 1 2)) => "23:01:02")
(check (clock-time->iso-8601 (make-clock-time 0 0 0)) => "00:00:00")
(check (clock-time->iso-8601 (make-clock-time 3 4 (+ 5 1/10))) => "03:04:05.1")

(check-catch 'type-error (clock-time->iso-8601 "23:01:02"))
(check-catch 'type-error (clock-time->iso-8601 230102))

(check-report)
