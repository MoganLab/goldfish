;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;  http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii json schema format-datetime)
  (import (scheme base)
    (scheme char)
    (liii json schema format-helpers)
    (liii string)
  ) ;import
  (export date? time? date-time? duration?)
  (begin
    ;; ---- shared helpers ---------------------------------------------------

    (define (digits? value start end)
      (and (<= start end)
        (< end (string-length value))
        (let loop
          ((index start))
          (cond ((> index end) #t)
                ((json-schema-ascii-digit? (string-ref value index)) (loop (+ index 1)))
                (else #f)
          ) ;cond
        ) ;let
      ) ;and
    ) ;define
    (define (digits->number value start end)
      (let loop
        ((index start) (result 0))
        (if (> index end)
          result
          (loop (+ index 1)
            (+ (* result 10) (- (char->integer (string-ref value index)) 48))
          ) ;loop
        ) ;if
      ) ;let
    ) ;define
    (define (index-of-char value char)
      (let loop
        ((index 0))
        (cond ((>= index (string-length value)) #f)
              ((char=? (string-ref value index) char) index)
              (else (loop (+ index 1)))
        ) ;cond
      ) ;let
    ) ;define

    ;; ---- date -------------------------------------------------------------

    (define (leap-year? year)
      (and (zero? (remainder year 4))
        (or (not (zero? (remainder year 100))) (zero? (remainder year 400)))
      ) ;and
    ) ;define
    (define (days-in-month year month)
      (cond ((= month 2) (if (leap-year? year) 29 28))
            ((or (= month 4) (= month 6) (= month 9) (= month 11)) 30)
            (else 31)
      ) ;cond
    ) ;define
    ;; A date is exactly YYYY-MM-DD with a real calendar day.
    (define (date? value)
      (and (= (string-length value) 10)
        (string=? (substring value 4 5) "-")
        (string=? (substring value 7 8) "-")
        (digits? value 0 3)
        (digits? value 5 6)
        (digits? value 8 9)
        (let ((year (digits->number value 0 3))
              (month (digits->number value 5 6))
              (day (digits->number value 8 9))
             ) ;
          (and (> year 0)
            (<= 1 month)
            (<= month 12)
            (<= 1 day)
            (<= day (days-in-month year month))
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    ;; ---- time -------------------------------------------------------------

    ;; A time is HH:MM:SS[.fraction](Z|z|(+|-)HH:MM).
    ;; The second may be 60 only at 23:59:60 UTC.
    (define (time? value)
      (let ((zone-index (time-zone-index value)))
        (and zone-index
          (let ((zone (substring value zone-index (string-length value))))
            (and (valid-zone? zone)
              (let* ((body (substring value 0 zone-index)) (dot (index-of-char body #\.)))
                (and (if dot (= dot 8) (= (string-length body) 8))
                  (string=? (substring body 2 3) ":")
                  (string=? (substring body 5 6) ":")
                  (digits? body 0 1)
                  (digits? body 3 4)
                  (digits? body 6 7)
                  (if dot (digits? body 9 (- (string-length body) 1)) #t)
                  (let ((hour (digits->number body 0 1))
                        (minute (digits->number body 3 4))
                        (second (digits->number body 6 7))
                       ) ;
                    (and (<= hour 23)
                      (<= minute 59)
                      (<= second 60)
                      (or (< second 60) (leap-second? hour minute zone))
                    ) ;and
                  ) ;let
                ) ;and
              ) ;let*
            ) ;and
          ) ;let
        ) ;and
      ) ;let
    ) ;define
    ;; Returns the index of the time zone designator or #f.
    (define (time-zone-index value)
      (let loop
        ((index 0))
        (cond ((>= index (string-length value)) #f)
              ((char=? (string-ref value index) #\Z) index)
              ((char=? (string-ref value index) #\z) index)
              ((char=? (string-ref value index) #\+) index)
              ((char=? (string-ref value index) #\-) index)
              (else (loop (+ index 1)))
        ) ;cond
      ) ;let
    ) ;define
    ;; The zone is Z, z or a signed HH:MM offset.
    (define (valid-zone? zone)
      (cond ((or (string=? zone "Z") (string=? zone "z")) #t)
            (else
              (and (= (string-length zone) 6)
                (or (char=? (string-ref zone 0) #\+) (char=? (string-ref zone 0) #\-))
                (string=? (substring zone 3 4) ":")
                (digits? zone 1 2)
                (digits? zone 4 5)
                (<= (digits->number zone 1 2) 23)
                (<= (digits->number zone 4 5) 59)
              ) ;and
            ) ;else
      ) ;cond
    ) ;define
    ;; Offset minutes of the local time, or 0 for Z.
    (define (zone-offset-minutes zone)
      (if (or (string=? zone "Z") (string=? zone "z"))
        0
        (let ((minutes (+ (* (digits->number zone 1 2) 60) (digits->number zone 4 5))))
          (if (char=? (string-ref zone 0) #\-) (- minutes) minutes)
        ) ;let
      ) ;if
    ) ;define
    ;; The UTC minute of day must be 23:59 for a leap second.
    (define (leap-second? hour minute zone)
      (let* ((local (+ (* hour 60) minute))
             (utc (modulo (- local (zone-offset-minutes zone)) 1440))
            ) ;
        (= utc 1439)
      ) ;let*
    ) ;define

    ;; ---- date-time --------------------------------------------------------

    (define (date-time? value)
      (let ((split (date-time-split value)))
        (and split (date? (car split)) (time? (cdr split)))
      ) ;let
    ) ;define
    ;; Splits at the date/time separator, which may be lower case.
    (define (date-time-split value)
      (let loop
        ((index 0))
        (cond ((>= index (string-length value)) #f)
              ((char=? (string-ref value index) #\T)
               (cons (substring value 0 index)
                 (substring value (+ index 1) (string-length value))
               ) ;cons
              ) ;
              ((char=? (string-ref value index) #\t)
               (cons (substring value 0 index)
                 (substring value (+ index 1) (string-length value))
               ) ;cons
              ) ;
              (else (loop (+ index 1)))
        ) ;cond
      ) ;let
    ) ;define

    ;; ---- duration ---------------------------------------------------------

    ;; An ISO 8601 duration such as P4DT12H30M5S.
    (define (duration? value)
      (and (> (string-length value) 1)
        (char=? (string-ref value 0) #\P)
        (duration-body? value 1)
      ) ;and
    ) ;define
    ;; Parses the components after the leading P.
    (define (duration-body? value index)
      (let ((week (duration-week? value index)))
        (if week
          (= week (string-length value))
          (if (and (< index (string-length value)) (char=? (string-ref value index) #\T))
            (duration-time? value (+ index 1))
            (let ((rest (duration-date? value index)))
              (if rest
                (if (string=? rest "")
                  #t
                  (and (char=? (string-ref rest 0) #\T) (duration-time? rest 1))
                ) ;if
                #f
              ) ;if
            ) ;let
          ) ;if
        ) ;if
      ) ;let
    ) ;define
    ;; Returns the number of characters consumed by a week component.
    (define (duration-week? value index)
      (let ((count (duration-number-length value index)))
        (if
          (and count
            (< (+ index count) (string-length value))
            (char=? (string-ref value (+ index count)) #\W)
          ) ;and
          (+ index count 1)
          #f
        ) ;if
      ) ;let
    ) ;define
    ;; Returns the number of digits of a component.
    (define (duration-number-length value index)
      (let loop
        ((index index) (count 0))
        (if (and (< index (string-length value))
              (json-schema-ascii-digit? (string-ref value index))
            ) ;and
          (loop (+ index 1) (+ count 1))
          (if (> count 0) count #f)
        ) ;if
      ) ;let
    ) ;define
    ;; Parses the date part and returns the remaining string.
    (define (duration-date? value index)
      (let ((year (duration-component value index #\Y)))
        (if year
          (let* ((after-year year)
                 (month (duration-component value after-year #\M))
                 (after-month (if month month after-year))
                 (day (duration-component value after-month #\D))
                 (after-day (if day day after-month))
                ) ;
            (if (and year (not month) (not day))
              (substring value after-year (string-length value))
              (if (and year month (not day))
                (substring value after-month (string-length value))
                (if (and year month day) (substring value after-day (string-length value)) #f)
              ) ;if
            ) ;if
          ) ;let*
          (let ((month (duration-component value index #\M)))
            (if month
              (let ((day (duration-component value month #\D)))
                (if day
                  (substring value day (string-length value))
                  (substring value month (string-length value))
                ) ;if
              ) ;let
              (let ((day (duration-component value index #\D)))
                (if day (substring value day (string-length value)) #f)
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    ;; Parses one "<digits><unit>" component.
    (define (duration-component value index unit)
      (let ((count (duration-number-length value index)))
        (if
          (and count
            (< (+ index count) (string-length value))
            (char=? (string-ref value (+ index count)) unit)
          ) ;and
          (+ index count 1)
          #f
        ) ;if
      ) ;let
    ) ;define
    ;; Parses the time part after the T.
    (define (duration-time? value index)
      (let ((hour (duration-component value index #\H)))
        (if hour
          (let ((minute (duration-component value hour #\M)))
            (if minute
              (let ((second (duration-component value minute #\S)))
                (if second (= second (string-length value)) (= minute (string-length value)))
              ) ;let
              (= hour (string-length value))
            ) ;if
          ) ;let
          (let ((minute (duration-component value index #\M)))
            (if minute
              (let ((second (duration-component value minute #\S)))
                (if second (= second (string-length value)) (= minute (string-length value)))
              ) ;let
              (let ((second (duration-component value index #\S)))
                (and second (= second (string-length value)))
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
