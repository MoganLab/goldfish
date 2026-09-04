(import (srfi srfi-19) (srfi srfi-78))
;; Known-value regressions.  Expected numbers were cross-checked against
;; Guile 3's (srfi srfi-19) over a 1950..2099 grid (identical output).
(check-set-mode! 'report-failed)

;; Julian / modified julian days (epoch counted from noon, like Guile).
(check (date->julian-day (make-date 0 0 0 0 1 1 2000 0)) => 4903089/2)
(check (date->modified-julian-day (make-date 0 0 0 0 1 1 2000 0)) => 51544)
(check (date->julian-day (make-date 0 0 0 0 1 1 1970 0)) => 4881175/2)

;; Week/year day arithmetic.
(check (date-week-day (make-date 0 0 0 0 1 1 1970 0)) => 4)
(check (date-year-day (make-date 0 0 0 0 1 1 1970 0)) => 1)
(check (date-week-day (make-date 0 0 0 0 5 3 2024 0)) => 2)
(check (date-year-day (make-date 0 0 0 0 5 3 2024 0)) => 65)
(check (date-week-number (make-date 0 0 0 0 29 2 2000 0) 1) => 8)

;; UTC/TAI conversion, leap-second table, and the pre-1972 identity rule.
;; The date->time-utc / time-tai round trip must be lossless.
(check (time-second (date->time-utc (make-date 0 0 0 0 1 1 1970 0))) => 0)
(check (time-second (date->time-tai (make-date 0 0 0 0 1 1 1970 0))) => 0)
(check (time-second (date->time-tai (make-date 0 0 0 0 1 1 1972 0))) => 63072010)
(check (time-second (date->time-tai (make-date 0 0 0 0 1 1 2000 0))) => 946684832)
(check (time-second (time-tai->time-utc (date->time-tai (make-date 0 0 0 0 1 1 2000 0)))) => 946684800)
(let ((d (time-utc->date (date->time-utc (make-date 0 0 0 0 5 3 2024 0)) 0)))
  (check (list (date-year d) (date-month d) (date-day d)) => '(2024 3 5)))

;; current-julian-day: no longer a stub; must agree with the date converter
;; for "now" in UTC.
(let* ((today (date->julian-day (current-date 0)))
       (cjd (current-julian-day)))
  (check (rational? cjd) => #t)
  (check (>= cjd today) => #t)
  (check (< cjd (+ today 2)) => #t))

;; date->string / string->date round trip.
(check (date->string
         (string->date "2024-03-05 13:07:09" "~Y-~m-~d ~H:~M:~S")
         "~Y-~m-~d ~H:~M:~S")
   => "2024-03-05 13:07:09")

(check-report)
(if (check-failed?) (exit -1))
