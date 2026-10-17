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

;; Copyright (C) 2026 Peter McGoron
;; Copyright (C) 2024-2026 Arvydas Silanskas
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

(define-library (liii date-time internal date-week)
  (import (scheme base))
  (export calc-date-iso-weekday calc-date-iso-week calc-date-iso-week-year)
  (begin

    (define (zeller-congruence year month day)
      (let* ((q day)
             (m (if (>= month 3) month (+ month 12)))
             (adj-year (if (>= month 3) year (- year 1)))
             (k (floor-remainder adj-year 100))
             (j (floor-quotient adj-year 100))
            ) ;
        (floor-remainder
          (+ q
            (floor-quotient (* 13 (+ m 1)) 5)
            k
            (floor-quotient k 4)
            (floor-quotient j 4)
            (- (* 2 j))
          ) ;+
          7
        ) ;floor-remainder
      ) ;let*
    ) ;define

    (define (calc-date-iso-weekday d date? date-ymd)
      (unless (date? d)
        (error 'type-error "date-iso-weekday: expected a date" d)
      ) ;unless
      (let-values (((year month day) (date-ymd d)))
        (let* ((num (zeller-congruence year month day)) (wd (floor-remainder (+ num 6) 7)))
          (if (= 0 wd) 7 wd)
        ) ;let*
      ) ;let-values
    ) ;define

    (define (iso-week-offset-for-jan1 year raw-make-date calc-iso-wd)
      (let ((wd (calc-iso-wd (raw-make-date year 1 1))))
        (if (>= 5 wd) (- wd 1) (- 1 wd))
      ) ;let
    ) ;define

    (define (calc-date-iso-week-info d date-ymd calc-iso-wd leap-year?
              days-since-start raw-make-date
            ) ;calc-date-iso-week-info
      (let-values (((year month day) (date-ymd d)))
        (let* ((weekday (calc-iso-wd d))
               (year-offset
                 (cond
                  ((and (= month 1) (<= day 3) (>= (- weekday day) 4)) -1)
                  ((and (= month 12) (>= day 29) (>= (- day weekday) 28)) 1)
                  (else 0)
                 ) ;cond
               ) ;year-offset
              ) ;
          (case year-offset
           ((1) (values (+ 1 year) 1))
           ((-1)
            (let* ((prev-y (- year 1))
                   (offset (iso-week-offset-for-jan1 prev-y raw-make-date calc-iso-wd))
                   (day-cnt (+ offset (if (leap-year? prev-y) 366 365) day))
                  ) ;
              (values prev-y (+ 1 (floor-quotient day-cnt 7)))
            ) ;let*
           ) ;
           (else
             (let* ((offset (iso-week-offset-for-jan1 year raw-make-date calc-iso-wd))
                    (day-cnt (+ (days-since-start d) offset))
                   ) ;
               (values year (+ 1 (floor-quotient day-cnt 7)))
             ) ;let*
           ) ;else
          ) ;case
        ) ;let*
      ) ;let-values
    ) ;define

    (define (calc-date-iso-week d date? date-ymd calc-iso-wd leap-year?
              days-since-start raw-make-date
            ) ;calc-date-iso-week
      (unless (date? d)
        (error 'type-error "date-iso-week: expected a date" d)
      ) ;unless
      (let-values (((y w)
                    (calc-date-iso-week-info d date-ymd calc-iso-wd leap-year?
                      days-since-start raw-make-date
                    ) ;calc-date-iso-week-info
                   ) ;
                  ) ;
        w
      ) ;let-values
    ) ;define

    (define (calc-date-iso-week-year d date? date-ymd calc-iso-wd leap-year?
              days-since-start raw-make-date
            ) ;calc-date-iso-week-year
      (unless (date? d)
        (error 'type-error "date-iso-week-year: expected a date" d)
      ) ;unless
      (let-values (((y w)
                    (calc-date-iso-week-info d date-ymd calc-iso-wd leap-year?
                      days-since-start raw-make-date
                    ) ;calc-date-iso-week-info
                   ) ;
                  ) ;
        y
      ) ;let-values
    ) ;define

  ) ;begin
) ;define-library
