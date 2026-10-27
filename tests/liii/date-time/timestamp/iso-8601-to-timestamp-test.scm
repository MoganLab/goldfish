(import (liii check)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; iso-8601->timestamp
;; 解析 ISO-8601 时间戳字符串为 timestamp 对象。
;;
;; 语法
;; ----
;; (iso-8601->timestamp str)
;;
;; 参数
;; ----
;; str : string?
;;
;; 返回值
;; ----
;; timestamp?
;;
;; 示例
;; ----
;; (iso-8601->timestamp "2026-01-02T03:04:05Z")

(let ((ts (iso-8601->timestamp "2026-01-02T03:04:05Z")))
  (check-true (timestamp? ts))
  (check (timestamp-year ts) => 2026)
  (check (timestamp-month ts) => 1)
  (check (timestamp-day ts) => 2)
  (check (timestamp-hour ts) => 3)
  (check (timestamp-minute ts) => 4)
  (check (timestamp-second ts) => 5))

(check-catch 'type-error (iso-8601->timestamp 12345))
(check-catch 'type-error (iso-8601->timestamp "2026-01-02"))

;; 带毫秒与 Z 后缀的真实会话时间戳格式
(check (timestamp->utc-posix-time (iso-8601->timestamp "2026-10-20T08:00:02.000Z"))
  => 1792483202)
(check-report)
