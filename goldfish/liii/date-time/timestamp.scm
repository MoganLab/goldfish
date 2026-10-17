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
    (liii date-time internal timestamp-types)
    (liii date-time internal timestamp-math)
    (liii date-time internal timestamp-iso)
    (liii date-time internal timestamp-conv)
    (liii date-time internal timestamp-cmp)
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

    (define (validate-time-in-tz ts)
      (let* ((local-tp (timestamp->local-timepoint ts))
             (tz (%timestamp-timezone ts))
             (offset (find-offset/wall tz local-tp))
             (fold (%timestamp-fold ts))
            ) ;
        (cond ((= 0 (vector-length offset))
               (error 'type-error
                 "date+clock-time->timestamp: invalid local time for timezone"
               ) ;error
              ) ;
              ((and (= 1 fold) (< (vector-length offset) 2))
               (error 'type-error "date+clock-time->timestamp: invalid fold for timestamp")
              ) ;
              (else #t)
        ) ;cond
      ) ;let*
    ) ;define

    (define (validate-leapsecond ts ok err)
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
          (validate-time-in-tz ts)
          (validate-leapsecond ts
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
          ) ;validate-leapsecond
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

    (define (ts-no-rules ts dt)
      (timestamp+/no-rules ts dt %make-timestamp %timestamp-date
        %timestamp-clock-time %timestamp-timezone
      ) ;timestamp+/no-rules
    ) ;define

    (define (timestamp-in-utc ts)
      (calc-timestamp-in-utc ts timestamp-timezone-offset ts-no-rules
        %make-timestamp %timestamp-date %timestamp-clock-time
      ) ;calc-timestamp-in-utc
    ) ;define

    (define (timestamp-in-timezone ts tz)
      (calc-timestamp-in-tz ts tz timestamp? %timestamp-timezone
        timestamp-in-utc ts-no-rules timestamp->local-timepoint
        date+clock-time->timestamp %timestamp-date %timestamp-clock-time
      ) ;calc-timestamp-in-tz
    ) ;define

    (define (moment->timestamp m tz)
      (calc-moment->timestamp m tz moment? timezone? date+clock-time->timestamp
        timestamp-in-timezone
      ) ;calc-moment->timestamp
    ) ;define

    (define (timestamp->moment ts)
      (calc-timestamp->moment ts timestamp? timestamp-in-utc
        timestamp->local-timepoint %timestamp-clock-time %timestamp-date
      ) ;calc-timestamp->moment
    ) ;define

    (define (timestamp->utc-posix-time ts)
      (calc-timestamp->posix ts timestamp? timestamp-in-timezone
        %timestamp-clock-time timestamp->local-timepoint
      ) ;calc-timestamp->posix
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
        (calc-posix->timestamp seconds date+clock-time->timestamp)
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

    (define (timestamp->iso-8601 ts)
      (format-timestamp-iso-8601 ts timestamp? timestamp-date timestamp-hour
        timestamp-minute timestamp-second timestamp-timezone-offset
      ) ;format-timestamp-iso-8601
    ) ;define

    (define (iso-8601->timestamp str)
      (parse-timestamp-iso-8601 str date+clock-time->timestamp)
    ) ;define

    (define (timestamp+ ts dt)
      (calc-timestamp+ ts dt timestamp? %timestamp-timezone %timestamp-date
        %timestamp-clock-time timestamp-in-utc timestamp-in-timezone
        validate-leapsecond %make-timestamp moment->timestamp timestamp->moment
      ) ;calc-timestamp+
    ) ;define

    (define (timestamp- ts dt)
      (calc-timestamp- ts dt timestamp+)
    ) ;define

    (define timestamp=?
      (make-timestamp-comparator zero? timestamp? timestamp->moment)
    ) ;define
    (define timestamp<?
      (make-timestamp-comparator negative? timestamp? timestamp->moment)
    ) ;define
    (define timestamp<=?
      (make-timestamp-comparator (lambda (c) (<= c 0)) timestamp? timestamp->moment)
    ) ;define
    (define timestamp>?
      (make-timestamp-comparator positive? timestamp? timestamp->moment)
    ) ;define
    (define timestamp>=?
      (make-timestamp-comparator (lambda (c) (>= c 0)) timestamp? timestamp->moment)
    ) ;define

  ) ;begin
) ;define-library
