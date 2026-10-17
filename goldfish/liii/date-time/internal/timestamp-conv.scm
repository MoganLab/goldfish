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

(define-library (liii date-time internal timestamp-conv)
  (import (scheme base)
    (liii date-time date)
    (liii date-time clock-time)
    (liii date-time moment)
    (liii date-time time-delta)
    (liii date-time timezone)
    (liii date-time internal leap-seconds)
    (liii date-time internal database)
    (liii date-time internal timestamp-math)
  ) ;import
  (export calc-timestamp-in-utc calc-timestamp-in-tz calc-moment->timestamp
    calc-timestamp->moment calc-timestamp->posix calc-posix->timestamp
  ) ;export
  (begin

    (define unix-epoch-rd (date->rata-die (make-date 1970 1 1)))

    (define (calc-timestamp-in-utc ts ts-offset ts-adj-no-rules make-raw ts-d ts-clk)
      (let* ((dt (time-delta-negate (ts-offset ts))) (ts-adj (ts-adj-no-rules ts dt)))
        (make-raw (ts-d ts-adj) (ts-clk ts-adj) (utc-timezone) 0)
      ) ;let*
    ) ;define

    (define (calc-timestamp-in-tz ts tz timestamp? ts-tz ts-in-utc
              ts-adj-no-rules ts-tp make-ts ts-d ts-clk
            ) ;calc-timestamp-in-tz
      (unless (and (timestamp? ts) (timezone? tz))
        (error 'type-error "timestamp-in-timezone: invalid parameters" ts tz)
      ) ;unless
      (if (eqv? tz (ts-tz ts))
        ts
        (let* ((utc-ts (ts-in-utc ts))
               (tp (ts-tp utc-ts))
               (range (find-offset/utc tz tp))
               (offset (time-range-offset range))
               (local-ts (ts-adj-no-rules utc-ts offset))
               (base-ts (make-ts (ts-d local-ts) (ts-clk local-ts) tz))
               (offsets-vec (find-offset/wall tz (ts-tp base-ts)))
               (fold?
                 (and (> (vector-length offsets-vec) 1)
                   (= (time-delta->seconds offset)
                     (time-delta->seconds (time-range-offset (vector-ref offsets-vec 1)))
                   ) ;=
                 ) ;and
               ) ;fold?
              ) ;
          (if fold? (make-ts (ts-d base-ts) (ts-clk base-ts) tz 1) base-ts)
        ) ;let*
      ) ;if
    ) ;define

    (define (calc-moment->timestamp m tz moment? timezone? make-ts ts-in-tz)
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
                 (utc-ts (make-ts d2 clk (utc-timezone)))
                ) ;
            (ts-in-tz utc-ts tz)
          ) ;let*
        ) ;let-values
      ) ;let*
    ) ;define

    (define (calc-timestamp->moment ts timestamp? ts-in-utc ts-tp ts-clk ts-d)
      (unless (timestamp? ts)
        (error 'type-error "timestamp->moment: expected a timestamp" ts)
      ) ;unless
      (let* ((utc-ts (ts-in-utc ts)) (tp (floor (ts-tp utc-ts))))
        (let-values (((leap? off) (leapsecond-info/utc tp)))
          (let* ((clk (ts-clk utc-ts))
                 (h (clock-time-hour clk))
                 (m (clock-time-minute clk))
                 (s (clock-time-second clk))
                 (total-secs (+ (* h 3600) (* m 60) s off))
                 (secs-full (floor total-secs))
                 (secs-frac (- total-secs secs-full))
                 (secs-int secs-full)
                 (d (ts-d utc-ts))
                 (date-diff (floor-quotient secs-int 86400))
                 (d2 (if (zero? date-diff) d (date+ d (days-delta date-diff))))
                 (sec-of-day (+ (floor-remainder secs-int 86400) secs-frac))
                ) ;
            (make-moment d2 sec-of-day)
          ) ;let*
        ) ;let-values
      ) ;let*
    ) ;define

    (define (calc-timestamp->posix ts timestamp? ts-in-tz ts-clk ts-tp)
      (unless (timestamp? ts)
        (error 'type-error "timestamp->utc-posix-time: expected a timestamp" ts)
      ) ;unless
      (let* ((utc-ts (ts-in-tz ts (utc-timezone)))
             (leap? (>= (clock-time-second (ts-clk utc-ts)) 60))
             (tp (ts-tp utc-ts))
            ) ;
        (if leap? (- tp 1) tp)
      ) ;let*
    ) ;define

    (define (calc-posix->timestamp seconds make-ts)
      (let* ((days (floor-quotient seconds 86400))
             (d (rata-die->date (+ unix-epoch-rd days)))
             (rem-sec (floor-remainder seconds 86400))
             (h (floor-quotient rem-sec 3600))
             (rem-sec2 (floor-remainder rem-sec 3600))
             (m (floor-quotient rem-sec2 60))
             (s (floor-remainder rem-sec2 60))
             (clk (make-clock-time h m s))
            ) ;
        (make-ts d clk (utc-timezone))
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
