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

(define-library (liii date-time date)
  (import (scheme base))
  (export make-date date? date-year date-month date-day date-ymd date->iso-8601
    iso-8601->date date-iso-weekday date-iso-week date-iso-week-year
    date->rata-die rata-die->date date->mjd mjd->date date=? date<? date<=?
    date>? date>=? leap-year? days-in-month-raw
  ) ;export
  (begin

    (define-record-type <date>
      (%make-date year month day)
      date?
      (year %date-year)
      (month %date-month)
      (day %date-day)
    ) ;define-record-type

    (define (leap-year? year)
      (unless (and (integer? year) (exact? year))
        (error 'type-error "leap-year?: year must be an exact integer" year)
      ) ;unless
      (cond
       ((not (zero? (floor-remainder year 4))) #f)
       ((not (zero? (floor-remainder year 100))) #t)
       (else (zero? (floor-remainder year 400)))
      ) ;cond
    ) ;define

    (define (days-in-month-raw is-leap-year month)
      (case month
       ((1 3 5 7 8 10 12) 31)
       ((4 6 9 11) 30)
       ((2) (if is-leap-year 29 28))
       (else #f)
      ) ;case
    ) ;define

    (define (valid-ymd? y m d)
      (and (integer? y)
        (exact? y)
        (integer? m)
        (exact? m)
        (integer? d)
        (exact? d)
        (<= 1 m 12)
        (let ((max-d (days-in-month-raw (leap-year? y) m)))
          (and max-d (<= 1 d max-d))
        ) ;let
      ) ;and
    ) ;define

    (define (make-date year month day)
      (unless (valid-ymd? year month day)
        (error 'type-error "make-date: invalid year, month or day" year month day)
      ) ;unless
      (%make-date year month day)
    ) ;define

    (define (date-year d)
      (unless (date? d)
        (error 'type-error "date-year: expected a date" d)
      ) ;unless
      (%date-year d)
    ) ;define

    (define (date-month d)
      (unless (date? d)
        (error 'type-error "date-month: expected a date" d)
      ) ;unless
      (%date-month d)
    ) ;define

    (define (date-day d)
      (unless (date? d)
        (error 'type-error "date-day: expected a date" d)
      ) ;unless
      (%date-day d)
    ) ;define

    (define (date-ymd d)
      (unless (date? d)
        (error 'type-error "date-ymd: expected a date" d)
      ) ;unless
      (values (%date-year d) (%date-month d) (%date-day d))
    ) ;define

    ;; Zeller's congruence
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

    (define (date-weekday d)
      (let-values (((year month day) (date-ymd d)))
        (let ((num (zeller-congruence year month day)))
          (floor-remainder (+ num 6) 7)
        ) ;let
      ) ;let-values
    ) ;define

    (define (date-iso-weekday d)
      (unless (date? d)
        (error 'type-error "date-iso-weekday: expected a date" d)
      ) ;unless
      (let ((wd (date-weekday d)))
        (if (= 0 wd) 7 wd)
      ) ;let
    ) ;define

    (define (days-since-year-start d)
      (let-values (((year month day) (date-ymd d)))
        (let ((is-leap (leap-year? year)))
          (let loop
            ((days 0) (i 1))
            (if (= i month)
              (+ days day)
              (loop (+ days (days-in-month-raw is-leap i)) (+ 1 i))
            ) ;if
          ) ;let
        ) ;let
      ) ;let-values
    ) ;define

    (define (apply-days-after-year-start year days)
      (let ((is-leap (leap-year? year)))
        (let loop
          ((days days) (m 1))
          (let ((max-d (days-in-month-raw is-leap m)))
            (if (> days max-d) (loop (- days max-d) (+ 1 m)) (make-date year m days))
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    (define (date-iso-week* d)
      (let-values (((year month day) (date-ymd d)))
        (let ((weekday (date-iso-weekday d)))
          (let ((year-offset
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
              (let* ((prev-year (- year 1))
                     (day-of-week-on-jan1 (date-iso-weekday (%make-date prev-year 1 1)))
                     (offset (if (>= 5 day-of-week-on-jan1)
                               (- day-of-week-on-jan1 1)
                               (- 1 day-of-week-on-jan1)
                             ) ;if
                     ) ;offset
                     (day-count (+ offset (if (leap-year? prev-year) 366 365) day))
                    ) ;
                (values prev-year (+ 1 (floor-quotient day-count 7)))
              ) ;let*
             ) ;
             (else
               (let* ((day-of-week-on-jan1 (date-iso-weekday (%make-date year 1 1)))
                      (offset (if (>= 5 day-of-week-on-jan1)
                                (- day-of-week-on-jan1 1)
                                (- 1 day-of-week-on-jan1)
                              ) ;if
                      ) ;offset
                      (day-count (+ (days-since-year-start d) offset))
                     ) ;
                 (values year (+ 1 (floor-quotient day-count 7)))
               ) ;let*
             ) ;else
            ) ;case
          ) ;let
        ) ;let
      ) ;let-values
    ) ;define

    (define (date-iso-week d)
      (unless (date? d)
        (error 'type-error "date-iso-week: expected a date" d)
      ) ;unless
      (let-values (((y w) (date-iso-week* d)))
        w
      ) ;let-values
    ) ;define

    (define (date-iso-week-year d)
      (unless (date? d)
        (error 'type-error "date-iso-week-year: expected a date" d)
      ) ;unless
      (let-values (((y w) (date-iso-week* d)))
        y
      ) ;let-values
    ) ;define

    (define (left-pad str c min-length)
      (let ((pad-size (max 0 (- min-length (string-length str)))))
        (if (> pad-size 0) (string-append (make-string pad-size c) str) str)
      ) ;let
    ) ;define

    (define (date->iso-8601 d)
      (unless (date? d)
        (error 'type-error "date->iso-8601: expected a date" d)
      ) ;unless
      (let-values (((y m day) (date-ymd d)))
        (string-append (if (< y 0) "-" "")
          (left-pad (number->string (abs y)) #\0 4)
          "-"
          (left-pad (number->string m) #\0 2)
          "-"
          (left-pad (number->string day) #\0 2)
        ) ;string-append
      ) ;let-values
    ) ;define

    (define (iso-8601->date str)
      (unless (string? str)
        (error 'type-error "iso-8601->date: expected a string" str)
      ) ;unless
      (let ((len (string-length str)))
        (let-values (((is-neg rem-str)
                      (if (and (> len 0) (char=? (string-ref str 0) #\-))
                        (values #t (substring str 1 len))
                        (values #f str)
                      ) ;if
                     ) ;
                    ) ;
          (let ((parts
                  (let loop
                    ((i 0) (start 0) (acc '()))
                    (cond
                     ((= i (string-length rem-str)) (reverse (cons (substring rem-str start i) acc)))
                     ((char=? (string-ref rem-str i) #\-)
                      (loop (+ i 1) (+ i 1) (cons (substring rem-str start i) acc))
                     ) ;
                     (else (loop (+ i 1) start acc))
                    ) ;cond
                  ) ;let
                ) ;parts
               ) ;
            (unless (= (length parts) 3)
              (error 'type-error "iso-8601->date: invalid format" str)
            ) ;unless
            (let ((y-str (car parts)) (m-str (cadr parts)) (d-str (caddr parts)))
              (let ((raw-y (string->number y-str))
                    (m (string->number m-str))
                    (d (string->number d-str))
                   ) ;
                (unless (and raw-y
                          m
                          d
                          (integer? raw-y)
                          (exact? raw-y)
                          (integer? m)
                          (exact? m)
                          (integer? d)
                          (exact? d)
                        ) ;and
                  (error 'type-error "iso-8601->date: invalid numbers" str)
                ) ;unless
                (let ((y (if is-neg (- raw-y) raw-y)))
                  (make-date y m d)
                ) ;let
              ) ;let
            ) ;let
          ) ;let
        ) ;let-values
      ) ;let
    ) ;define

    (define (%date-compare-impl d1 d2)
      (let-values (((y1 m1 day1) (date-ymd d1)) ((y2 m2 day2) (date-ymd d2)))
        (cond ((< y1 y2) -1)
              ((> y1 y2) 1)
              ((< m1 m2) -1)
              ((> m1 m2) 1)
              ((< day1 day2) -1)
              ((> day1 day2) 1)
              (else 0)
        ) ;cond
      ) ;let-values
    ) ;define

    (define (make-transitive-comparator pred)
      (lambda (d1 d2 . rest)
        (unless (date? d1)
          (error 'type-error "date comparison: expected a date" d1)
        ) ;unless
        (unless (date? d2)
          (error 'type-error "date comparison: expected a date" d2)
        ) ;unless
        (let loop
          ((prev d1) (curr d2) (rem rest))
          (and (pred (%date-compare-impl prev curr))
            (if (null? rem)
              #t
              (let ((nxt (car rem)))
                (unless (date? nxt)
                  (error 'type-error "date comparison: expected a date" nxt)
                ) ;unless
                (loop curr nxt (cdr rem))
              ) ;let
            ) ;if
          ) ;and
        ) ;let
      ) ;lambda
    ) ;define

    (define date=? (make-transitive-comparator zero?))
    (define date<? (make-transitive-comparator negative?))
    (define date<=? (make-transitive-comparator (lambda (c) (<= c 0))))
    (define date>? (make-transitive-comparator positive?))
    (define date>=? (make-transitive-comparator (lambda (c) (>= c 0))))

    ;; Rata Die conversions
    (define days/4y (+ (* 4 365) 1))
    (define days/100y (+ (* 100 365) 24))
    (define days/400y (+ (* 4 days/100y) 1))

    (define (date->rata-die* d)
      (define (rd-for-year year)
        (let* ((curr-year-adjust (if (leap-year? year) -366 -365))
               (part400s (* days/400y (floor-quotient year 400)))
               (y (floor-remainder year 400))
               (part100s (* days/100y (floor-quotient y 100)))
               (y (floor-remainder y 100))
               (part4s (* days/4y (floor-quotient y 4)))
               (y (floor-remainder y 4))
               (rest (* y 365))
              ) ;
          (+ part400s part100s part4s rest curr-year-adjust)
        ) ;let*
      ) ;define
      (let ((year (date-year d)))
        (+ (rd-for-year year) (days-since-year-start d))
      ) ;let
    ) ;define

    (define (date->rata-die d)
      (unless (date? d)
        (error 'type-error "date->rata-die: expected a date" d)
      ) ;unless
      (let ((year (date-year d)))
        (if (>= year 0)
          (date->rata-die* d)
          (let* ((m (date-month d))
                 (day (date-day d))
                 (n (+ 1 (floor-quotient (abs year) 400)))
                 (rd1
                   (date->rata-die* (make-date (+ year (* n 400)) m day))
                 ) ;rd1
                 (rd2
                   (date->rata-die* (make-date (+ 1 (* n 400)) 1 1))
                 ) ;rd2
                ) ;
            (+ 1 (- rd1 rd2))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (rata-die->date* rd)
      (define (floor-quotient/capped num denom max-val)
        (min (floor-quotient num denom) max-val)
      ) ;define
      (define (floor-remainder/capped num denom max-val)
        (let ((v (min (floor-quotient num denom) max-val)))
          (- num (* v denom))
        ) ;let
      ) ;define
      (let* ((rd (- rd 1))
             (part400s (* 400 (floor-quotient rd days/400y)))
             (rd (floor-remainder rd days/400y))
             (part100s (* 100 (floor-quotient/capped rd days/100y 3)))
             (rd (floor-remainder/capped rd days/100y 3))
             (part4s (* 4 (floor-quotient/capped rd days/4y 24)))
             (rd (floor-remainder/capped rd days/4y 24))
             (rest (floor-quotient/capped rd 365 3))
             (rd (floor-remainder/capped rd 365 3))
             (year (+ part400s part100s part4s rest))
            ) ;
        (apply-days-after-year-start (+ 1 year) (+ 1 rd))
      ) ;let*
    ) ;define

    (define (rata-die->date rd)
      (unless (and (integer? rd) (exact? rd))
        (error 'type-error "rata-die->date: expected an exact integer" rd)
      ) ;unless
      (if (>= rd 1)
        (rata-die->date* rd)
        (let* ((n (+ 1 (floor-quotient (abs rd) days/400y)))
               (d* (rata-die->date* (+ (* n days/400y) rd)))
               (year (- (date-year d*) (* n 400)))
              ) ;
          (%make-date year (date-month d*) (date-day d*))
        ) ;let*
      ) ;if
    ) ;define

    (define mjd-rd-offset (- 2400000 1721424))

    (define (date->mjd d)
      (unless (date? d)
        (error 'type-error "date->mjd: expected a date" d)
      ) ;unless
      (- (date->rata-die d) mjd-rd-offset)
    ) ;define

    (define (mjd->date mjd)
      (unless (and (integer? mjd) (exact? mjd))
        (error 'type-error "mjd->date: expected an exact integer" mjd)
      ) ;unless
      (rata-die->date (+ mjd-rd-offset mjd))
    ) ;define

  ) ;begin
) ;define-library
