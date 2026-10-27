;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 会话选择器的相对时间格式化。
(define-library (liii pi code modes interactive session-selector-time)
  (import (scheme base) (liii string) (liii date-time))
  (export session-selector-format-age)
  (begin

    (define (%activity-posix timestamp)
      ;; 会话时间戳均为 UTC ISO-8601；避免依赖当前 date-time 的
      ;; iso-8601->timestamp 传递依赖，按日期与时分秒直接换算 POSIX 秒。
      (let* ((date (iso-8601->date (substring timestamp 0 10)))
             (days (- (date->rata-die date) (date->rata-die (make-date 1970 1 1))))
             (hour (string->number (substring timestamp 11 13)))
             (minute (string->number (substring timestamp 14 16)))
             (second (string->number (substring timestamp 17 19)))
            ) ;
        (+ (* days 86400) (* hour 3600) (* minute 60) second)
      ) ;let*
    ) ;define

    (define* (session-selector-format-age timestamp (now-posix #f))
      (catch #t
        (lambda ()
          (let* ((then (%activity-posix timestamp))
                 (now (or now-posix (timestamp->utc-posix-time (current-utc-timestamp))))
                 (minutes
                   (max 0 (floor (/ (- now then) 60)))
                 ) ;minutes
                 (hours (quotient minutes 60))
                 (days (quotient hours 24))
                ) ;
            (cond ((< minutes 1) "now")
                  ((< minutes 60) (string-append (number->string minutes) "m"))
                  ((< hours 24) (string-append (number->string hours) "h"))
                  ((< days 7) (string-append (number->string days) "d"))
                  ((< days 30) (string-append (number->string (quotient days 7)) "w"))
                  ((< days 365) (string-append (number->string (quotient days 30)) "mo"))
                  (else (string-append (number->string (quotient days 365)) "y"))
            ) ;cond
          ) ;let*
        ) ;lambda
        (lambda args "")
      ) ;catch
    ) ;define*

  ) ;begin
) ;define-library
