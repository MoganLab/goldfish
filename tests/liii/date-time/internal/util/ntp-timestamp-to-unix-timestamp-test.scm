;; ntp-timestamp->unix-timestamp
;; 将 NTP 秒时间戳换算为 UNIX 秒时间戳。
;;
;; 语法
;; ----
;; (ntp-timestamp->unix-timestamp ntp)
;;
;; 参数
;; ----
;; ntp : integer? NTP 纪元（1900-01-01）起的秒数。
;;
;; 返回值
;; ----
;; integer? UNIX 纪元（1970-01-01）起的秒数。

(import (liii check) (liii date-time internal util))

(check-set-mode! 'report-failed)

(check (ntp-timestamp->unix-timestamp 2208988800) => 0)
(check (ntp-timestamp->unix-timestamp 2272060800) => 63072000)

(check-report)
