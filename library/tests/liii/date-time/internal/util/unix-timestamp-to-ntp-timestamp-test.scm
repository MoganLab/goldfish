;; unix-timestamp->ntp-timestamp
;; 将 UNIX 秒时间戳换算为 NTP 秒时间戳。
;;
;; 语法
;; ----
;; (unix-timestamp->ntp-timestamp unix)
;;
;; 参数
;; ----
;; unix : integer? UNIX 纪元（1970-01-01）起的秒数。
;;
;; 返回值
;; ----
;; integer? NTP 纪元（1900-01-01）起的秒数。

(import (liii check) (liii date-time internal util))

(check-set-mode! 'report-failed)

(check (unix-timestamp->ntp-timestamp 0) => 2208988800)
(check (unix-timestamp->ntp-timestamp 63072000) => 2272060800)

(check-report)
