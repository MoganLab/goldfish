;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

;; Copyright (C) 2024-2026 Arvydas Silanskas
;; Copyright (C) 2026 Peter McGoron
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;

(define-library (liii date-time timestamp)
  (import (scheme base)
    (scheme case-lambda)
    (liii date-time date)
    (liii date-time clock-time)
    (liii date-time moment)
    (liii date-time time-delta)
    (liii date-time timezone)
    (liii date-time internal util)
    (liii date-time internal database)
    (liii date-time internal leap-seconds)
  ) ;import
  (export make-timestamp date+clock-time->timestamp timestamp? timestamp-date
    timestamp-clock-time timestamp-timezone timestamp-fold timestamp-year
    timestamp-month timestamp-day timestamp-ymd timestamp-hour timestamp-minute
    timestamp-second timestamp-hms moment->timestamp timestamp->moment
    posix-time->utc-timestamp timestamp->utc-posix-time timestamp-in-timezone
    timestamp-timezone-offset timestamp-timezone-dst?
    timestamp-timezone-abbreviation timestamp->iso-8601 iso-8601->timestamp
    timestamp+ timestamp- timestamp=? timestamp<? timestamp<=? timestamp>?
    timestamp>=?
  ) ;export
  (begin

    (define-record-type <timestamp>
      (%make-timestamp date time timezone fold)
      timestamp?
      (date %timestamp-date)
      (time %timestamp-clock-time)
      (timezone %timestamp-timezone)
      (fold %timestamp-fold)
    ) ;define-record-type

    (define unix-epoch-rd (date->rata-die (make-date 1970 1 1)))

    (define (timestamp-date ts)
      (unless (timestamp? ts)
        (error 'type-error "timestamp-date: expected a timestamp" ts)
      ) ;unless
      (%timestamp-date ts)
    ) ;define

    (define (timestamp-clock-time ts)
      (unless (timestamp? ts)
        (error 'type-error "timestamp-clock-time: expected a timestamp" ts)
      ) ;unless
      (%timestamp-clock-time ts)
    ) ;define

    (define (timestamp-timezone ts)
      (unless (timestamp? ts)
        (error 'type-error "timestamp-timezone: expected a timestamp" ts)
      ) ;unless
      (%timestamp-timezone ts)
    ) ;define

    (define (timestamp-fold ts)
      (unless (timestamp? ts)
        (error 'type-error "timestamp-fold: expected a timestamp" ts)
      ) ;unless
      (%timestamp-fold ts)
    ) ;define

    (define (timestamp-ymd ts)
      (unless (timestamp? ts)
        (error 'type-error "timestamp-ymd: expected a timestamp" ts)
      ) ;unless
      (date-ymd (%timestamp-date ts))
    ) ;define

    (define (timestamp-year ts)
      (unless (timestamp? ts)
        (error 'type-error "timestamp-year: expected a timestamp" ts)
      ) ;unless
      (date-year (%timestamp-date ts))
    ) ;define

    (define (timestamp-month ts)
      (unless (timestamp? ts)
        (error 'type-error "timestamp-month: expected a timestamp" ts)
      ) ;unless
      (date-month (%timestamp-date ts))
    ) ;define

    (define (timestamp-day ts)
      (unless (timestamp? ts)
        (error 'type-error "timestamp-day: expected a timestamp" ts)
      ) ;unless
      (date-day (%timestamp-date ts))
    ) ;define

    (define (timestamp-hms ts)
      (unless (timestamp? ts)
        (error 'type-error "timestamp-hms: expected a timestamp" ts)
      ) ;unless
      (clock-time-hms (%timestamp-clock-time ts))
    ) ;define

    (define (timestamp-hour ts)
      (unless (timestamp? ts)
        (error 'type-error "timestamp-hour: expected a timestamp" ts)
      ) ;unless
      (clock-time-hour (%timestamp-clock-time ts))
    ) ;define

    (define (timestamp-minute ts)
      (unless (timestamp? ts)
        (error 'type-error "timestamp-minute: expected a timestamp" ts)
      ) ;unless
      (clock-time-minute (%timestamp-clock-time ts))
    ) ;define

    (define (timestamp-second ts)
      (unless (timestamp? ts)
        (error 'type-error "timestamp-second: expected a timestamp" ts)
      ) ;unless
      (clock-time-second (%timestamp-clock-time ts))
    ) ;define

    (define (timestamp->local-timepoint ts)
      (let* ((days (- (date->rata-die (%timestamp-date ts)) unix-epoch-rd))
             (clk (%timestamp-clock-time ts))
            ) ;
        (+ (* days 86400)
          (* (clock-time-hour clk) 3600)
          (* (clock-time-minute clk) 60)
          (clock-time-second clk)
        ) ;+
      ) ;let*
    ) ;define

    (define (validate-timestamp-time-in-timezone ts ok err-bad-time err-bad-fold)
      (let* ((local-tp (timestamp->local-timepoint ts))
             (tz (%timestamp-timezone ts))
             (offset (find-offset/wall tz local-tp))
             (fold (%timestamp-fold ts))
            ) ;
        (cond ((= 0 (vector-length offset)) (err-bad-time))
              ((and (= 1 fold) (< (vector-length offset) 2)) (err-bad-fold))
              (else (ok))
        ) ;cond
      ) ;let*
    ) ;define

    (define (validate-timestamp-leapsecond ts ok err)
      (if (< (timestamp-second ts) 60)
        (ok)
        (let* ((ts-utc (timestamp-in-utc ts))
               (tp (floor (timestamp->local-timepoint ts-utc)))
              ) ;
          (let-values (((leap? off) (leapsecond-info/utc tp)))
            (if leap? (ok) (err))
          ) ;let-values
        ) ;let*
      ) ;if
    ) ;define

    (define date+clock-time->timestamp
      (case-lambda
       ((date time timezone) (date+clock-time->timestamp date time timezone 0))
       ((date time timezone fold)
        (unless (and (date? date)
                  (clock-time? time)
                  (timezone? timezone)
                  (integer? fold)
                  (or (= fold 0) (= fold 1))
                ) ;and
          (error 'type-error
            "date+clock-time->timestamp: invalid parameters"
            date
            time
            timezone
            fold
          ) ;error
        ) ;unless
        (let ((ts (%make-timestamp date time timezone fold)))
          (validate-timestamp-time-in-timezone ts
            (lambda () #t)
            (lambda ()
              (error 'type-error
                "date+clock-time->timestamp: invalid local time for timezone"
                date
                time
                timezone
                fold
              ) ;error
            ) ;lambda
            (lambda ()
              (error 'type-error
                "date+clock-time->timestamp: invalid fold for timestamp"
                date
                time
                timezone
                fold
              ) ;error
            ) ;lambda
          ) ;validate-timestamp-time-in-timezone
          (validate-timestamp-leapsecond ts
            (lambda () #t)
            (lambda ()
              (error 'type-error
                "date+clock-time->timestamp: invalid leap second"
                date
                time
                timezone
                fold
              ) ;error
            ) ;lambda
          ) ;validate-timestamp-leapsecond
          ts
        ) ;let
       ) ;
      ) ;case-lambda
    ) ;define

    (define make-timestamp
      (case-lambda
       ((year month day hour minute second timezone)
        (date+clock-time->timestamp (make-date year month day)
          (make-clock-time hour minute second)
          timezone
          0
        ) ;date+clock-time->timestamp
       ) ;
       ((year month day hour minute second timezone fold)
        (date+clock-time->timestamp (make-date year month day)
          (make-clock-time hour minute second)
          timezone
          fold
        ) ;date+clock-time->timestamp
       ) ;
      ) ;case-lambda
    ) ;define

    (define (time-delta->seconds dt)
      (+ (* (time-delta-days dt) 86400)
        (* (time-delta-hours dt) 3600)
        (* (time-delta-minutes dt) 60)
        (time-delta-seconds dt)
      ) ;+
    ) ;define

    (define (timestamp+/no-rules ts dt)
      (let* ((leap? (>= (timestamp-second ts) 60))
             (d (%timestamp-date ts))
             (secs
               (+ (* 3600 (+ (timestamp-hour ts) (time-delta-hours dt)))
                 (* 60 (+ (timestamp-minute ts) (time-delta-minutes dt)))
                 (timestamp-second ts)
                 (time-delta-seconds dt)
               ) ;+
             ) ;secs
             (secs (if leap? (- secs 1) secs))
             (secs-full (floor secs))
             (secs-frac (- secs secs-full))
             (secs secs-full)
             (days-diff (floor-quotient secs 86400))
             (d2 (if (zero? days-diff) d (date+ d (days-delta days-diff))))
             (rem-sec (floor-remainder secs 86400))
             (h (floor-quotient rem-sec 3600))
             (rem-sec2 (floor-remainder rem-sec 3600))
             (m (floor-quotient rem-sec2 60))
             (s (+ (floor-remainder rem-sec2 60) secs-frac))
             (s (if leap? (+ s 1) s))
             (clk (make-clock-time h m s))
            ) ;
        (%make-timestamp d2 clk (%timestamp-timezone ts) 0)
      ) ;let*
    ) ;define

    (define (timestamp-timezone-offset ts)
      (unless (timestamp? ts)
        (error 'type-error "timestamp-timezone-offset: expected a timestamp" ts)
      ) ;unless
      (let* ((local-tp (timestamp->local-timepoint ts))
             (tz (%timestamp-timezone ts))
             (fold (%timestamp-fold ts))
             (offsets-vec (find-offset/wall tz local-tp))
            ) ;
        (time-range-offset
          (vector-ref offsets-vec (min fold (- (vector-length offsets-vec) 1)))
        ) ;time-range-offset
      ) ;let*
    ) ;define

    (define (timestamp-timezone-abbreviation ts)
      (unless (timestamp? ts)
        (error 'type-error "timestamp-timezone-abbreviation: expected a timestamp" ts)
      ) ;unless
      (let* ((local-tp (timestamp->local-timepoint ts))
             (tz (%timestamp-timezone ts))
             (fold (%timestamp-fold ts))
             (offsets-vec (find-offset/wall tz local-tp))
            ) ;
        (time-range-designation
          (vector-ref offsets-vec (min fold (- (vector-length offsets-vec) 1)))
        ) ;time-range-designation
      ) ;let*
    ) ;define

    (define (timestamp-timezone-dst? ts)
      (unless (timestamp? ts)
        (error 'type-error "timestamp-timezone-dst?: expected a timestamp" ts)
      ) ;unless
      (let* ((local-tp (timestamp->local-timepoint ts))
             (tz (%timestamp-timezone ts))
             (fold (%timestamp-fold ts))
             (offsets-vec (find-offset/wall tz local-tp))
            ) ;
        (time-range-dst?
          (vector-ref offsets-vec (min fold (- (vector-length offsets-vec) 1)))
        ) ;time-range-dst?
      ) ;let*
    ) ;define

    (define (timestamp-in-utc ts)
      (let* ((dt (time-delta-negate (timestamp-timezone-offset ts)))
             (ts-adj (timestamp+/no-rules ts dt))
            ) ;
        (%make-timestamp (%timestamp-date ts-adj)
          (%timestamp-clock-time ts-adj)
          (utc-timezone)
          0
        ) ;%make-timestamp
      ) ;let*
    ) ;define

    (define (timestamp-in-timezone ts tz)
      (unless (and (timestamp? ts) (timezone? tz))
        (error 'type-error "timestamp-in-timezone: invalid parameters" ts tz)
      ) ;unless
      (let ((curr-tz (%timestamp-timezone ts)))
        (if (eqv? tz curr-tz)
          ts
          (let* ((utc-ts (timestamp-in-utc ts))
                 (tp (timestamp->local-timepoint utc-ts))
                 (range (find-offset/utc tz tp))
                 (offset (time-range-offset range))
                 (local-ts (timestamp+/no-rules utc-ts offset))
                 (base-ts (date+clock-time->timestamp (%timestamp-date local-ts)
                            (%timestamp-clock-time local-ts)
                            tz
                          ) ;date+clock-time->timestamp
                 ) ;base-ts
                 (offsets-vec (find-offset/wall tz (timestamp->local-timepoint base-ts)))
                 (fold?
                   (and (> (vector-length offsets-vec) 1)
                     (= (time-delta->seconds offset)
                       (time-delta->seconds (time-range-offset (vector-ref offsets-vec 1)))
                     ) ;=
                   ) ;and
                 ) ;fold?
                ) ;
            (if fold?
              (date+clock-time->timestamp (%timestamp-date base-ts)
                (%timestamp-clock-time base-ts)
                tz
                1
              ) ;date+clock-time->timestamp
              base-ts
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (moment->timestamp m tz)
      (unless (and (moment? m) (timezone? tz))
        (error 'type-error "moment->timestamp: invalid parameters" m tz)
      ) ;unless
      (let* ((d (moment-date m))
             (secs (moment-second-of-day m))
             (days (- (date->rata-die d) unix-epoch-rd))
             (tp (+ (* days 86400) secs))
            ) ;
        (let-values (((leap? off) (leapsecond-info/tai tp)))
          (let* ((secs (- secs off))
                 (secs-full (floor secs))
                 (secs-frac (- secs secs-full))
                 (secs secs-full)
                 (date-diff (floor-quotient secs 86400))
                 (d2 (if (zero? date-diff) d (date+ d (days-delta date-diff))))
                 (rem-sec (floor-remainder secs 86400))
                 (h (floor-quotient rem-sec 3600))
                 (rem-sec2 (floor-remainder rem-sec 3600))
                 (min (floor-quotient rem-sec2 60))
                 (s (+ (floor-remainder rem-sec2 60) secs-frac))
                 (s (if leap? (+ s 1) s))
                 (clk (make-clock-time h min s))
                 (utc-ts (date+clock-time->timestamp d2 clk (utc-timezone)))
                ) ;
            (timestamp-in-timezone utc-ts tz)
          ) ;let*
        ) ;let-values
      ) ;let*
    ) ;define

    (define (timestamp->moment ts)
      (unless (timestamp? ts)
        (error 'type-error "timestamp->moment: expected a timestamp" ts)
      ) ;unless
      (let* ((utc-ts (timestamp-in-utc ts))
             (tp (floor (timestamp->local-timepoint utc-ts)))
            ) ;
        (let-values (((leap? off) (leapsecond-info/utc tp)))
          (let* ((h (timestamp-hour utc-ts))
                 (m (timestamp-minute utc-ts))
                 (s (timestamp-second utc-ts))
                 (total-secs (+ (* h 3600) (* m 60) s off))
                 (secs-full (floor total-secs))
                 (secs-frac (- total-secs secs-full))
                 (secs-int secs-full)
                 (d (timestamp-date utc-ts))
                 (date-diff (floor-quotient secs-int 86400))
                 (d2 (if (zero? date-diff) d (date+ d (days-delta date-diff))))
                 (sec-of-day (+ (floor-remainder secs-int 86400) secs-frac))
                ) ;
            (make-moment d2 sec-of-day)
          ) ;let*
        ) ;let-values
      ) ;let*
    ) ;define

    (define (timestamp->utc-posix-time ts)
      (unless (timestamp? ts)
        (error 'type-error "timestamp->utc-posix-time: expected a timestamp" ts)
      ) ;unless
      (let* ((utc-ts (timestamp-in-timezone ts (utc-timezone)))
             (leap? (>= (timestamp-second utc-ts) 60))
             (tp (timestamp->local-timepoint utc-ts))
            ) ;
        (if leap? (- tp 1) tp)
      ) ;let*
    ) ;define

    (define posix-time->utc-timestamp
      (case-lambda
       ((seconds)
        (unless (and (exact? seconds) (rational? seconds))
          (error 'type-error
            "posix-time->utc-timestamp: expected an exact rational"
            seconds
          ) ;error
        ) ;unless
        (let* ((days (floor-quotient seconds 86400))
               (d (rata-die->date (+ unix-epoch-rd days)))
               (rem-sec (floor-remainder seconds 86400))
               (h (floor-quotient rem-sec 3600))
               (rem-sec2 (floor-remainder rem-sec 3600))
               (m (floor-quotient rem-sec2 60))
               (s (floor-remainder rem-sec2 60))
               (clk (make-clock-time h m s))
              ) ;
          (date+clock-time->timestamp d clk (utc-timezone))
        ) ;let*
       ) ;
       ((seconds nano-seconds)
        (unless (and (integer? seconds)
                  (exact? seconds)
                  (integer? nano-seconds)
                  (exact? nano-seconds)
                ) ;and
          (error 'type-error
            "posix-time->utc-timestamp: expected exact integers"
            seconds
            nano-seconds
          ) ;error
        ) ;unless
        (posix-time->utc-timestamp (+ seconds (/ nano-seconds 1000000000)))
       ) ;
      ) ;case-lambda
    ) ;define

    (define (format-second-iso s)
      (let* ((int-part (floor s))
             (frac-part (- s int-part))
             (base-str (left-pad (number->string (exact int-part)) #\0 2))
            ) ;
        (if (zero? frac-part)
          base-str
          (let* ((frac-flt (exact->inexact frac-part)) (s-str (number->string frac-flt)))
            (string-append base-str (substring s-str 1))
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (timestamp->iso-8601 ts)
      (unless (timestamp? ts)
        (error 'type-error "timestamp->iso-8601: expected a timestamp" ts)
      ) ;unless
      (let* ((off (timestamp-timezone-offset ts))
             (h (time-delta-hours off))
             (m (time-delta-minutes off))
             (tz-str
               (if (and (zero? h) (zero? m))
                 "Z"
                 (let ((sign (if (negative? h) "-" "+")))
                   (string-append sign
                     (left-pad (number->string (abs h)) #\0 2)
                     ":"
                     (left-pad (number->string (abs m)) #\0 2)
                   ) ;string-append
                 ) ;let
               ) ;if
             ) ;tz-str
            ) ;
        (string-append (date->iso-8601 (%timestamp-date ts))
          "T"
          (left-pad (number->string (timestamp-hour ts)) #\0 2)
          ":"
          (left-pad (number->string (timestamp-minute ts)) #\0 2)
          ":"
          (format-second-iso (timestamp-second ts))
          tz-str
        ) ;string-append
      ) ;let*
    ) ;define

    (define (iso-8601->timestamp str)
      (unless (string? str)
        (error 'type-error "iso-8601->timestamp: expected a string" str)
      ) ;unless
      (let ((t-pos
              (let loop
                ((i 0))
                (cond ((= i (string-length str)) #f)
                      ((or (char=? (string-ref str i) #\T) (char=? (string-ref str i) #\t)) i)
                      (else (loop (+ i 1)))
                ) ;cond
              ) ;let
            ) ;t-pos
           ) ;
        (unless t-pos
          (error 'type-error "iso-8601->timestamp: invalid format (missing T)" str)
        ) ;unless
        (let* ((date-part (substring str 0 t-pos))
               (time-rest (substring str (+ t-pos 1)))
               (d (iso-8601->date date-part))
              ) ;
          (let-values (((time-str tz)
                        (cond
                         ((string=? (substring time-rest (- (string-length time-rest) 1)) "Z")
                          (values (substring time-rest 0 (- (string-length time-rest) 1)) (utc-timezone))
                         ) ;
                         ((or (string-contains-char? time-rest #\+) (string-contains-char? time-rest #\-))
                          (let* ((sign-idx (find-tz-sign time-rest))
                                 (t-str (substring time-rest 0 sign-idx))
                                 (tz-str (substring time-rest sign-idx))
                                 (sign (if (char=? (string-ref tz-str 0) #\-) -1 1))
                                 (tz-body (substring tz-str 1))
                                 (colon (string-index-char tz-body #\:))
                                ) ;
                            (if colon
                              (let ((zh (string->number (substring tz-body 0 colon)))
                                    (zm (string->number (substring tz-body (+ colon 1))))
                                   ) ;
                                (values t-str
                                  (utc-offset-timezone (time-delta+ (hours-delta (* sign zh)) (minutes-delta (* sign zm)))
                                  ) ;utc-offset-timezone
                                ) ;values
                              ) ;let
                              (values t-str (utc-timezone))
                            ) ;if
                          ) ;let*
                         ) ;
                         (else (values time-rest (utc-timezone)))
                        ) ;cond
                       ) ;
                      ) ;
            (let ((clk (iso-8601->clock-time time-str)))
              (date+clock-time->timestamp d clk tz)
            ) ;let
          ) ;let-values
        ) ;let*
      ) ;let
    ) ;define

    (define (string-contains-char? str ch)
      (let loop
        ((i 0))
        (cond ((= i (string-length str)) #f)
              ((char=? (string-ref str i) ch) #t)
              (else (loop (+ i 1)))
        ) ;cond
      ) ;let
    ) ;define

    (define (string-index-char str ch)
      (let loop
        ((i 0))
        (cond ((= i (string-length str)) #f)
              ((char=? (string-ref str i) ch) i)
              (else (loop (+ i 1)))
        ) ;cond
      ) ;let
    ) ;define

    (define (find-tz-sign str)
      (let loop
        ((i (- (string-length str) 1)))
        (cond ((< i 0) 0)
              ((or (char=? (string-ref str i) #\+) (char=? (string-ref str i) #\-)) i)
              (else (loop (- i 1)))
        ) ;cond
      ) ;let
    ) ;define

    (define (moment+seconds m s+)
      (let* ((d (moment-date m))
             (sec (+ (moment-second-of-day m) s+))
             (sec-full (floor sec))
             (sec-frac (- sec sec-full))
             (sec-int sec-full)
             (days+ (floor-quotient sec-int 86400))
             (d2 (if (zero? days+) d (date+ d (days-delta days+))))
             (rem-sec (+ (floor-remainder sec-int 86400) sec-frac))
            ) ;
        (make-moment d2 rem-sec)
      ) ;let*
    ) ;define

    (define (timestamp+ ts dt)
      (unless (and (timestamp? ts) (time-delta? dt))
        (error 'type-error "timestamp+: expected timestamp and time-delta" ts dt)
      ) ;unless
      (let* ((tz (%timestamp-timezone ts))
             (ts-utc (timestamp-in-utc ts))
             (d (%timestamp-date ts-utc))
             (clk (%timestamp-clock-time ts-utc))
             ;; add years, months, weeks, days
             (date-dt (time-delta+ (years-delta (time-delta-years dt))
                        (months-delta (time-delta-months dt))
                        (weeks-delta (time-delta-weeks dt))
                        (days-delta (time-delta-days dt))
                      ) ;time-delta+
             ) ;date-dt
             (d2 (date+ d date-dt))
             ;; add hours and minutes
             (old-mins (+ (* 60 (clock-time-hour clk)) (clock-time-minute clk)))
             (diff-mins (+ (* 60 (time-delta-hours dt)) (time-delta-minutes dt)))
             (total-mins (+ old-mins diff-mins))
             (days-diff (floor-quotient total-mins 1440))
             (d3 (if (zero? days-diff) d2 (date+ d2 (days-delta days-diff))))
             (rem-mins (floor-remainder total-mins 1440))
             (h (floor-quotient rem-mins 60))
             (m (floor-remainder rem-mins 60))
             (clk2 (make-clock-time h m (clock-time-second clk)))
             (ts2
               (let* ((t (%make-timestamp d3 clk2 (utc-timezone) 0)))
                 (validate-timestamp-leapsecond t
                   (lambda () t)
                   (lambda ()
                     (%make-timestamp d3
                       (make-clock-time h m (- (clock-time-second clk) 1))
                       (utc-timezone)
                       0
                     ) ;%make-timestamp
                   ) ;lambda
                 ) ;validate-timestamp-leapsecond
               ) ;let*
             ) ;ts2
             ;; seconds addition via moment
             (ts3
               (if (zero? (time-delta-seconds dt))
                 ts2
                 (moment->timestamp (moment+seconds (timestamp->moment ts2) (time-delta-seconds dt))
                   (utc-timezone)
                 ) ;moment->timestamp
               ) ;if
             ) ;ts3
            ) ;
        (timestamp-in-timezone ts3 tz)
      ) ;let*
    ) ;define

    (define (timestamp- ts dt)
      (unless (and (timestamp? ts) (time-delta? dt))
        (error 'type-error "timestamp-: expected timestamp and time-delta" ts dt)
      ) ;unless
      (timestamp+ ts (time-delta-negate dt))
    ) ;define

    (define (%timestamp-compare-impl ts1 ts2)
      (let ((m1 (timestamp->moment ts1)) (m2 (timestamp->moment ts2)))
        (cond ((moment<? m1 m2) -1)
              ((moment>? m1 m2) 1)
              (else 0)
        ) ;cond
      ) ;let
    ) ;define

    (define (make-transitive-comparator pred)
      (lambda (t1 t2 . rest)
        (unless (timestamp? t1)
          (error 'type-error "timestamp comparison: expected a timestamp" t1)
        ) ;unless
        (unless (timestamp? t2)
          (error 'type-error "timestamp comparison: expected a timestamp" t2)
        ) ;unless
        (let loop
          ((prev t1) (curr t2) (rem rest))
          (and (pred (%timestamp-compare-impl prev curr))
            (if (null? rem)
              #t
              (let ((nxt (car rem)))
                (unless (timestamp? nxt)
                  (error 'type-error "timestamp comparison: expected a timestamp" nxt)
                ) ;unless
                (loop curr nxt (cdr rem))
              ) ;let
            ) ;if
          ) ;and
        ) ;let
      ) ;lambda
    ) ;define

    (define timestamp=? (make-transitive-comparator zero?))
    (define timestamp<? (make-transitive-comparator negative?))
    (define timestamp<=? (make-transitive-comparator (lambda (c) (<= c 0))))
    (define timestamp>? (make-transitive-comparator positive?))
    (define timestamp>=? (make-transitive-comparator (lambda (c) (>= c 0))))

  ) ;begin
) ;define-library
