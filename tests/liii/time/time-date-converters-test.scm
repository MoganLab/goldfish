(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; Time/Date Converters
;; 时间和日期之间的转换函数。
;;
;; 包含的函数
;; --------
;; time-utc->date, date->time-utc
;; time-utc->time-tai, time-tai->time-utc
;; time-utc->time-monotonic, time-monotonic->time-utc
;; time-tai->time-monotonic, time-monotonic->time-tai
;; time-tai->date, date->time-tai
;; time-monotonic->date, date->time-monotonic
;; date->julian-day, date->modified-julian-day

;; time-utc->date basic (UTC)
(let* ((t (make-time TIME-UTC 0 0))
       (d (time-utc->date t 0)))
  (check (date-year d) => 1970)
  (check (date-month d) => 1)
  (check (date-day d) => 1)
  (check (date-hour d) => 0)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => 0)
) ;let*

;; time-utc->date with positive tz offset (+8)
(let* ((t (make-time TIME-UTC 0 0))
       (d (time-utc->date t 28800)))
  (check (date-year d) => 1970)
  (check (date-month d) => 1)
  (check (date-day d) => 1)
  (check (date-hour d) => 8)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => 28800)
) ;let*

;; date->time-utc basic
(let* ((d (make-date 0 0 0 8 1 1 1970 28800))
       (t (date->time-utc d)))
  (check (time-type t) => TIME-UTC)
  (check (time-second t) => 0)
  (check (time-nanosecond t) => 0)
) ;let*

;; round-trip date -> time -> date with same tz-offset
(let* ((d1 (make-date 123456789 45 30 14 25 12 2023 28800))
       (t (date->time-utc d1))
       (d2 (time-utc->date t (date-zone-offset d1))))
  (check (date-year d2) => (date-year d1))
  (check (date-month d2) => (date-month d1))
  (check (date-day d2) => (date-day d1))
  (check (date-hour d2) => (date-hour d1))
  (check (date-minute d2) => (date-minute d1))
  (check (date-second d2) => (date-second d1))
  (check (date-nanosecond d2) => (date-nanosecond d1))
  (check (date-zone-offset d2) => (date-zone-offset d1))
) ;let*

;; time-utc->time-tai / time-tai->time-utc basic
(let* ((t-utc (make-time TIME-UTC 123456789 1483228800))
       (t-tai (time-utc->time-tai t-utc)))
  (check (time-type t-tai) => TIME-TAI)
  (check (time-second t-tai) => 1483228837)
  (check (time-nanosecond t-tai) => 123456789)
  (let ((t-utc2 (time-tai->time-utc t-tai)))
    (check (time-type t-utc2) => TIME-UTC)
    (check (time-second t-utc2) => 1483228800)
    (check (time-nanosecond t-utc2) => 123456789)
  ) ;let
) ;let*

;; time-utc->time-monotonic / time-monotonic->time-utc basic
(let* ((t-utc (make-time TIME-UTC 123456789 42))
       (t-mon (time-utc->time-monotonic t-utc)))
  (check (time-type t-mon) => TIME-MONOTONIC)
  (check (time-second t-mon) => 42)
  (check (time-nanosecond t-mon) => 123456789)
  (let ((t-utc2 (time-monotonic->time-utc t-mon)))
    (check (time-type t-utc2) => TIME-UTC)
    (check (time-second t-utc2) => 42)
    (check (time-nanosecond t-utc2) => 123456789)
  ) ;let
) ;let*

;; date->julian-day / date->modified-julian-day basic (UTC epoch)
(let ((d (make-date 0 0 0 0 1 1 1970 0)))
  (check (date->julian-day d) => 4881175/2)
  (check (date->modified-julian-day d) => 40587)
) ;let

;; converter error conditions
(check-catch 'wrong-type-arg
  (time-utc->date (make-time TIME-TAI 0 0) 0)
) ;check-catch
(check-catch 'wrong-type-arg
  (date->time-utc "not-a-date")
) ;check-catch

(check-report)
