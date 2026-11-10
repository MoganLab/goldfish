;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai time)
  (import (scheme base) (liii string) (liii date-time))
  (export pi-iso-8601-utc-now pi-current-time-millis pi-current-time-seconds
    pi-current-monotonic-millis pi-current-monotonic-seconds
  ) ;export
  (begin

    (define (%pad-two-digits n)
      (string-pad (number->string n) 2 #\0)
    ) ;define

    (define (%pad-three-digits n)
      (string-pad (number->string n) 3 #\0)
    ) ;define

    ;; 当前 UTC 的 POSIX 时间（精确有理数，含小数秒）
    (define (%current-utc-posix-time)
      (timestamp->utc-posix-time (current-utc-timestamp))
    ) ;define

    (define (pi-iso-8601-utc-now)
      (let* ((ts (current-utc-timestamp))
             (posix (timestamp->utc-posix-time ts))
             (year (number->string (timestamp-year ts)))
             (month (%pad-two-digits (timestamp-month ts)))
             (day (%pad-two-digits (timestamp-day ts)))
             (hour (%pad-two-digits (timestamp-hour ts)))
             (minute (%pad-two-digits (timestamp-minute ts)))
             (second (%pad-two-digits (floor (timestamp-second ts))))
             (millis
               (%pad-three-digits
                 (floor (* 1000 (- posix (floor posix))))
               ) ;%pad-three-digits
             ) ;millis
            ) ;
        (string-append year "-" month "-" day "T" hour ":" minute ":" second "."
          millis "Z"
        ) ;string-append
      ) ;let*
    ) ;define

    (define (pi-current-time-millis)
      (floor (* 1000 (%current-utc-posix-time)))
    ) ;define

    (define (pi-current-time-seconds)
      (floor (%current-utc-posix-time))
    ) ;define

    (define (pi-current-monotonic-millis)
      (current-monotonic-millis)
    ) ;define

    (define (pi-current-monotonic-seconds)
      (current-monotonic-seconds)
    ) ;define

  ) ;begin
) ;define-library
