(import (scheme char) (liii check) (liii pi ai event) (liii date-time))

(check-set-mode! 'report-failed)

;; pi-iso-8601-utc-now
;; 获取当前 UTC 时间的 ISO 8601 格式字符串。
;; 输出固定为 YYYY-MM-DDTHH:MM:SS.mmmZ：UTC、T 分隔、三位毫秒、Z 结尾。
(let ((now (pi-iso-8601-utc-now)))
  (check-true (string? now))
  (check (string-length now) => 24)
  (check (string-ref now 4) => #\-)
  (check (string-ref now 7) => #\-)
  (check (string-ref now 10) => #\T)
  (check (string-ref now 13) => #\:)
  (check (string-ref now 16) => #\:)
  (check (string-ref now 19) => #\.)
  (check (string-ref now 23) => #\Z)
  ;; 分隔符之外的位置必须全部是数字
  (let loop
    ((i 0))
    (when (< i 24)
      (unless (memv i '(4 7 10 13 16 19 23))
        (check-true (char-numeric? (string-ref now i)))
      ) ;unless
      (loop (+ i 1))
    ) ;when
  ) ;let
  ;; 毫秒字段固定三位
  (check (string-length (substring now 20 23)) => 3)
  ;; 组件范围校验
  (let ((year (string->number (substring now 0 4)))
        (month (string->number (substring now 5 7)))
        (day (string->number (substring now 8 10)))
        (hour (string->number (substring now 11 13)))
        (minute (string->number (substring now 14 16)))
        (second (string->number (substring now 17 19)))
        (millis (string->number (substring now 20 23)))
       ) ;
    (check-true (and (>= year 2024) (< year 2100)))
    (check-true (and (>= month 1) (<= month 12)))
    (check-true (and (>= day 1) (<= day 31)))
    (check-true (and (>= hour 0) (<= hour 23)))
    (check-true (and (>= minute 0) (<= minute 59)))
    (check-true (and (>= second 0) (<= second 60)))
    (check-true (and (>= millis 0) (<= millis 999)))
    ;; 与 (liii date-time) 的 UTC 时间源一致（允许秒级边界滑动）
    (let* ((before (timestamp->utc-posix-time (current-utc-timestamp)))
           (parsed (timestamp->utc-posix-time (iso-8601->timestamp now)))
           (after (timestamp->utc-posix-time (current-utc-timestamp)))
          ) ;
      (check-true (exact? parsed))
      (check-true (>= parsed (- before 1)))
      (check-true (<= parsed (+ after 1)))
      ;; 毫秒字段与解析出的小数秒一致
      (let* ((frac (- parsed (floor parsed)))
             (parsed-millis (floor (* 1000 frac)))
            ) ;
        (check parsed-millis => millis)
      ) ;let*
    ) ;let*
  ) ;let
) ;let

(check-report)
