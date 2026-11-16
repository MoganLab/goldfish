;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai time)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii date-time)
    (liii pi ai abort)
  ) ;import
  (export pi-iso-8601-utc-now pi-current-time-millis pi-current-time-seconds
    pi-current-monotonic-millis pi-current-monotonic-seconds
    pi-format-timeout-reason pi-make-stream-progress-cb
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

    (define (pi-format-timeout-reason raw-msg timeout-sec)
      (if (and (string? raw-msg)
            (not (string-null? raw-msg))
            (string-contains? (string-downcase raw-msg) "timed out")
          ) ;and
        (if (string-starts? raw-msg "Request timed out")
          raw-msg
          (string-append "Request timed out: " raw-msg)
        ) ;if
        (string-append "Request timed out"
          (if (number? timeout-sec)
            (string-append " after " (number->string timeout-sec) "s")
            ""
          ) ;if
          (if (and (string? raw-msg) (not (string-null? raw-msg)))
            (string-append " (" raw-msg ")")
            ""
          ) ;if
        ) ;string-append
      ) ;if
    ) ;define

    (define (pi-make-stream-progress-cb eff-idle last-act on-idle on-abort)
      (lambda (dlt dln ult uln)
        (cond ((pi-aborted?) (on-abort) #f)
              ((and eff-idle
                 (> (- (pi-current-monotonic-seconds) (vector-ref last-act 0)) eff-idle)
               ) ;and
               (on-idle)
               #f
              ) ;
              (else #t)
        ) ;cond
      ) ;lambda
    ) ;define

  ) ;begin
) ;define-library
