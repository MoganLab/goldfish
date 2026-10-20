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

(define-library (liii date-time format)
  (import (scheme base)
    (scheme char)
    (liii base)
    (liii string)
    (liii date-time date)
    (liii date-time clock-time)
    (liii date-time moment)
    (liii date-time timezone)
    (liii date-time timestamp)
    (liii date-time gregorian)
    (liii date-time time-delta)
  ) ;import
  (export date-time-format date-time-parse)
  (begin

    (define ABBR-WEEKDAY (vector "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

    (define LONG-WEEKDAY
      (vector "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")
    ) ;define

    (define ABBR-MONTH
      (vector "" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct"
        "Nov" "Dec"
      ) ;vector
    ) ;define

    (define LONG-MONTH
      (vector "" "January" "February" "March" "April" "May" "June" "July"
        "August" "September" "October" "November" "December"
      ) ;vector
    ) ;define

    (define (format-tz-offset off with-colon?)
      (let* ((h (time-delta-hours off))
             (m (time-delta-minutes off))
             (s (time-delta-seconds off))
             (neg? (or (negative? h) (negative? m) (negative? s)))
             (sign-str (if neg? "-" "+"))
             (abs-h (abs h))
             (abs-m (abs m))
            ) ;
        (if with-colon?
          (string-append sign-str
            (string-pad (number->string abs-h) 2 #\0)
            ":"
            (string-pad (number->string abs-m) 2 #\0)
          ) ;string-append
          (string-append sign-str
            (string-pad (number->string abs-h) 2 #\0)
            (string-pad (number->string abs-m) 2 #\0)
          ) ;string-append
        ) ;if
      ) ;let*
    ) ;define

    (define (string-ci-prefix? str pos prefix)
      (let* ((plen (string-length prefix)) (slen (string-length str)))
        (and (<= (+ pos plen) slen)
          (let loop
            ((i 0))
            (cond ((= i plen) #t)
                  ((char=? (char-downcase (string-ref str (+ pos i)))
                     (char-downcase (string-ref prefix i))
                   ) ;char=?
                   (loop (+ i 1))
                  ) ;
                  (else #f)
            ) ;cond
          ) ;let
        ) ;and
      ) ;let*
    ) ;define

    (define (date-time-format obj . opt-fmt)
      (let ((fmt (if (null? opt-fmt) "%Y-%m-%d %H:%M:%S" (car opt-fmt))))
        (unless (string? fmt)
          (error 'type-error "date-time-format: format-string must be a string" fmt)
        ) ;unless

        ;; 提取分量
        (let-values (((year month day hour minute second tz-offset tz-name iso-wday yearday)
                      (cond
                       ((timestamp? obj)
                        (let* ((d (timestamp-date obj))
                               (y (date-year d))
                               (jan1 (make-date y 1 1))
                               (yd (+ 1 (- (date->rata-die d) (date->rata-die jan1))))
                              ) ;
                          (values y
                            (date-month d)
                            (date-day d)
                            (timestamp-hour obj)
                            (timestamp-minute obj)
                            (timestamp-second obj)
                            (timestamp-timezone-offset obj)
                            (timestamp-timezone-abbreviation obj)
                            (date-iso-weekday d)
                            yd
                          ) ;values
                        ) ;let*
                       ) ;
                       ((date? obj)
                        (let* ((y (date-year obj))
                               (jan1 (make-date y 1 1))
                               (yd (+ 1 (- (date->rata-die obj) (date->rata-die jan1))))
                              ) ;
                          (values y (date-month obj) (date-day obj) 0 0 0 #f #f (date-iso-weekday obj) yd)
                        ) ;let*
                       ) ;
                       ((clock-time? obj)
                        (values 0
                          1
                          1
                          (clock-time-hour obj)
                          (clock-time-minute obj)
                          (clock-time-second obj)
                          #f
                          #f
                          #f
                          #f
                        ) ;values
                       ) ;
                       ((moment? obj)
                        (let* ((d (moment-date obj))
                               (y (date-year d))
                               (jan1 (make-date y 1 1))
                               (yd (+ 1 (- (date->rata-die d) (date->rata-die jan1))))
                               (sod (moment-second-of-day obj))
                               (sod-floor (exact (floor sod)))
                               (h (quotient sod-floor 3600))
                               (m (quotient (modulo sod-floor 3600) 60))
                               (s (+ (modulo sod-floor 60) (- sod sod-floor)))
                              ) ;
                          (values y (date-month d) (date-day d) h m s #f #f (date-iso-weekday d) yd)
                        ) ;let*
                       ) ;
                       (else (error 'type-error "date-time-format: unsupported date/time object" obj))
                      ) ;cond
                     ) ;
                    ) ;

          (let* ((flen (string-length fmt))
                 (out (open-output-string))
                 (wday (if (and iso-wday (= iso-wday 7)) 0 iso-wday))
                ) ;

            (define (format-directive ch)
              (case ch
               ((#\Y) (display (string-pad (number->string year) 4 #\0) out))
               ((#\y) (display (string-pad (number->string (modulo year 100)) 2 #\0) out))
               ((#\m) (display (string-pad (number->string month) 2 #\0) out))
               ((#\d) (display (string-pad (number->string day) 2 #\0) out))
               ((#\e) (display (string-pad (number->string day) 2 #\space) out))
               ((#\j) (display (string-pad (number->string (or yearday 0)) 3 #\0) out))
               ((#\F)
                (display (date-time-format-extracted year month day hour minute
                           second tz-offset tz-name iso-wday yearday wday
                           "%Y-%m-%d"
                         ) ;date-time-format-extracted
                  out
                ) ;display
               ) ;
               ((#\H) (display (string-pad (number->string hour) 2 #\0) out))
               ((#\I)
                (let ((h (modulo hour 12)))
                  (display
                    (string-pad (number->string (if (= h 0) 12 h)) 2 #\0)
                    out
                  ) ;display
                ) ;let
               ) ;
               ((#\M) (display (string-pad (number->string minute) 2 #\0) out))
               ((#\S) (display (string-pad (number->string (exact (floor second))) 2 #\0) out))
               ((#\f)
                (let* ((frac (- second (floor second))) (micros (exact (round (* frac 1000000)))))
                  (display (string-pad (number->string micros) 6 #\0) out)
                ) ;let*
               ) ;
               ((#\p) (display (if (< hour 12) "AM" "PM") out))
               ((#\P) (display (if (< hour 12) "am" "pm") out))
               ((#\T)
                (display (date-time-format-extracted year month day hour minute
                           second tz-offset tz-name iso-wday yearday wday
                           "%H:%M:%S"
                         ) ;date-time-format-extracted
                  out
                ) ;display
               ) ;
               ((#\R)
                (display (date-time-format-extracted year month day hour minute
                           second tz-offset tz-name iso-wday yearday wday
                           "%H:%M"
                         ) ;date-time-format-extracted
                  out
                ) ;display
               ) ;
               ((#\a) (display (if wday (vector-ref ABBR-WEEKDAY wday) "") out))
               ((#\A) (display (if wday (vector-ref LONG-WEEKDAY wday) "") out))
               ((#\b #\h)
                (display (if (and (>= month 1) (<= month 12)) (vector-ref ABBR-MONTH month) "")
                  out
                ) ;display
               ) ;
               ((#\B)
                (display (if (and (>= month 1) (<= month 12)) (vector-ref LONG-MONTH month) "")
                  out
                ) ;display
               ) ;
               ((#\w) (display (if wday (number->string wday) "") out))
               ((#\u) (display (if iso-wday (number->string iso-wday) "") out))
               ((#\z)
                (if tz-offset (display (format-tz-offset tz-offset #f) out) (display "" out))
               ) ;
               ((#\Z) (if tz-name (display tz-name out) (display "" out)))
               ((#\c)
                (display (date-time-format-extracted year month day hour minute
                           second tz-offset tz-name iso-wday yearday wday
                           "%a %b %e %H:%M:%S %Y"
                         ) ;date-time-format-extracted
                  out
                ) ;display
               ) ;
               ((#\x)
                (display (date-time-format-extracted year month day hour minute
                           second tz-offset tz-name iso-wday yearday wday
                           "%m/%d/%y"
                         ) ;date-time-format-extracted
                  out
                ) ;display
               ) ;
               ((#\X)
                (display (date-time-format-extracted year month day hour minute
                           second tz-offset tz-name iso-wday yearday wday
                           "%H:%M:%S"
                         ) ;date-time-format-extracted
                  out
                ) ;display
               ) ;
               ((#\%) (write-char #\% out))
               (else (error 'value-error "date-time-format: invalid directive" ch))
              ) ;case
            ) ;define

            (let loop
              ((i 0))
              (cond ((>= i flen) (get-output-string out))
                    ((char=? (string-ref fmt i) #\%)
                     (let ((next-i (+ i 1)))
                       (if (>= next-i flen)
                         (error 'value-error
                           "date-time-format: unfinished directive at end of format"
                           fmt
                         ) ;error
                         (let ((ch (string-ref fmt next-i)))
                           (cond
                            ((char=? ch #\:)
                             (let ((after-colon (+ next-i 1)))
                               (if (and (< after-colon flen) (char=? (string-ref fmt after-colon) #\z))
                                 (begin
                                   (if tz-offset (display (format-tz-offset tz-offset #t) out) (display "" out))
                                   (loop (+ after-colon 1))
                                 ) ;begin
                                 (error 'value-error "date-time-format: invalid directive %:" fmt)
                               ) ;if
                             ) ;let
                            ) ;
                            (else (format-directive ch) (loop (+ next-i 1)))
                           ) ;cond
                         ) ;let
                       ) ;if
                     ) ;let
                    ) ;
                    (else (write-char (string-ref fmt i) out) (loop (+ i 1)))
              ) ;cond
            ) ;let
          ) ;let*
        ) ;let-values
      ) ;let
    ) ;define

    (define (date-time-format-extracted year month day hour minute second
              tz-offset tz-name iso-wday yearday wday fmt
            ) ;date-time-format-extracted
      (let* ((flen (string-length fmt)) (out (open-output-string)))
        (let loop
          ((i 0))
          (cond ((>= i flen) (get-output-string out))
                ((char=? (string-ref fmt i) #\%)
                 (let ((next-i (+ i 1)))
                   (if (>= next-i flen)
                     (error 'value-error "date-time-format: unfinished directive")
                     (let ((ch (string-ref fmt next-i)))
                       (case ch
                        ((#\Y) (display (string-pad (number->string year) 4 #\0) out))
                        ((#\y) (display (string-pad (number->string (modulo year 100)) 2 #\0) out))
                        ((#\m) (display (string-pad (number->string month) 2 #\0) out))
                        ((#\d) (display (string-pad (number->string day) 2 #\0) out))
                        ((#\e) (display (string-pad (number->string day) 2 #\space) out))
                        ((#\H) (display (string-pad (number->string hour) 2 #\0) out))
                        ((#\M) (display (string-pad (number->string minute) 2 #\0) out))
                        ((#\S) (display (string-pad (number->string (exact (floor second))) 2 #\0) out))
                        ((#\a) (display (if wday (vector-ref ABBR-WEEKDAY wday) "") out))
                        ((#\b)
                         (display (if (and (>= month 1) (<= month 12)) (vector-ref ABBR-MONTH month) "")
                           out
                         ) ;display
                        ) ;
                        (else (write-char ch out))
                       ) ;case
                       (loop (+ next-i 1))
                     ) ;let
                   ) ;if
                 ) ;let
                ) ;
                (else (write-char (string-ref fmt i) out) (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    ;; ====================
    ;; date-time-parse 实现
    ;; ====================

    (define (date-time-parse input-str . opts)
      (let* ((fmt (if (null? opts) "%Y-%m-%d %H:%M:%S" (car opts)))
             (default-tz
               (if (or (null? opts) (null? (cdr opts))) (system-timezone) (cadr opts))
             ) ;default-tz
            ) ;
        (unless (string? input-str)
          (error 'type-error "date-time-parse: input-str must be a string" input-str)
        ) ;unless
        (unless (string? fmt)
          (error 'type-error "date-time-parse: format-string must be a string" fmt)
        ) ;unless
        (unless (timezone? default-tz)
          (error 'type-error
            "date-time-parse: default-tz must be a timezone object"
            default-tz
          ) ;error
        ) ;unless

        (let ((in-len (string-length input-str))
              (fmt-len (string-length fmt))
              (year #f)
              (year-2digit #f)
              (month #f)
              (day #f)
              (hour #f)
              (hour12 #f)
              (ampm #f)
              (minute #f)
              (second #f)
              (microsecond #f)
              (tz-delta #f)
              (year-day #f)
             ) ;

          (define (expect-char pos ch)
            (if (and (< pos in-len) (char=? (string-ref input-str pos) ch))
              (+ pos 1)
              (error 'value-error
                "date-time-parse: char mismatch"
                (list input-str fmt pos ch)
              ) ;error
            ) ;if
          ) ;define

          (define (read-digits pos min-n max-n)
            (let loop
              ((i pos) (count 0))
              (if (and (< i in-len) (< count max-n) (char-numeric? (string-ref input-str i)))
                (loop (+ i 1) (+ count 1))
                (if (< count min-n)
                  (error 'value-error
                    "date-time-parse: expected digits"
                    (list input-str pos count min-n)
                  ) ;error
                  (values (string->number (substring input-str pos i)) i)
                ) ;if
              ) ;if
            ) ;let
          ) ;define

          (define (read-fixed-digits pos n)
            (read-digits pos n n)
          ) ;define

          (define (read-microsecond pos)
            (let loop
              ((i pos) (count 0))
              (if (and (< i in-len) (< count 6) (char-numeric? (string-ref input-str i)))
                (loop (+ i 1) (+ count 1))
                (if (= count 0)
                  (error 'value-error
                    "date-time-parse: expected microsecond digits"
                    (list input-str pos)
                  ) ;error
                  (let* ((raw-str (substring input-str pos i))
                         (padded (string-pad-right raw-str 6 #\0))
                        ) ;
                    (values (string->number padded) i)
                  ) ;let*
                ) ;if
              ) ;if
            ) ;let
          ) ;define

          (define (match-vector-name pos vec start-idx)
            (let ((len (vector-length vec)))
              (let loop
                ((i start-idx) (best-idx #f) (best-len 0))
                (if (= i len)
                  (if best-idx
                    (values best-idx (+ pos best-len))
                    (error 'value-error "date-time-parse: name mismatch" (list input-str pos))
                  ) ;if
                  (let ((name (vector-ref vec i)))
                    (if (and (string? name)
                          (> (string-length name) 0)
                          (string-ci-prefix? input-str pos name)
                        ) ;and
                      (let ((nlen (string-length name)))
                        (if (> nlen best-len) (loop (+ i 1) i nlen) (loop (+ i 1) best-idx best-len))
                      ) ;let
                      (loop (+ i 1) best-idx best-len)
                    ) ;if
                  ) ;let
                ) ;if
              ) ;let
            ) ;let
          ) ;define

          (define (parse-tz pos)
            (cond ((>= pos in-len)
                   (error 'value-error "date-time-parse: expected timezone offset" input-str)
                  ) ;
                  ((char=? (string-ref input-str pos) #\Z)
                   (set! tz-delta (make-time-delta 0 0 0 0 0 0 0))
                   (+ pos 1)
                  ) ;
                  ((or (char=? (string-ref input-str pos) #\+)
                     (char=? (string-ref input-str pos) #\-)
                   ) ;or
                   (let* ((sign (if (char=? (string-ref input-str pos) #\-) -1 1)) (pos1 (+ pos 1)))
                     (let-values (((hh pos2) (read-fixed-digits pos1 2)))
                       (let ((pos3
                               (if (and (< pos2 in-len) (char=? (string-ref input-str pos2) #\:))
                                 (+ pos2 1)
                                 pos2
                               ) ;if
                             ) ;pos3
                            ) ;
                         (let-values (((mm pos4) (read-fixed-digits pos3 2)))
                           (set! tz-delta (make-time-delta 0 0 0 0 (* sign hh) (* sign mm) 0))
                           pos4
                         ) ;let-values
                       ) ;let
                     ) ;let-values
                   ) ;let*
                  ) ;
                  (else (error 'value-error "date-time-parse: invalid timezone format" input-str))
            ) ;cond
          ) ;define

          ;; 遍历 fmt
          (let parse-loop
            ((fi 0) (ii 0))
            (cond ((>= fi fmt-len)
                   (if (< ii in-len)
                     (error 'value-error "date-time-parse: extra characters in input" input-str)
                     #t
                   ) ;if
                  ) ;
                  ((char=? (string-ref fmt fi) #\%)
                   (let ((next-fi (+ fi 1)))
                     (if (>= next-fi fmt-len)
                       (error 'value-error "date-time-parse: unfinished directive in fmt" fmt)
                       (let ((ch (string-ref fmt next-fi)))
                         (case ch
                          ((#\:)
                           (let ((after-colon (+ next-fi 1)))
                             (if (and (< after-colon fmt-len) (char=? (string-ref fmt after-colon) #\z))
                               (let ((next-ii (parse-tz ii)))
                                 (parse-loop (+ after-colon 1) next-ii)
                               ) ;let
                               (error 'value-error "date-time-parse: invalid directive %:" fmt)
                             ) ;if
                           ) ;let
                          ) ;
                          ((#\Y)
                           (let-values (((val next-ii) (read-digits ii 1 4)))
                             (set! year val)
                             (parse-loop (+ next-fi 1) next-ii)
                           ) ;let-values
                          ) ;
                          ((#\y)
                           (let-values (((val next-ii) (read-fixed-digits ii 2)))
                             (set! year-2digit val)
                             (parse-loop (+ next-fi 1) next-ii)
                           ) ;let-values
                          ) ;
                          ((#\m)
                           (let-values (((val next-ii) (read-digits ii 1 2)))
                             (set! month val)
                             (parse-loop (+ next-fi 1) next-ii)
                           ) ;let-values
                          ) ;
                          ((#\d #\e)
                           (let ((skip-ii
                                   (if (and (< ii in-len) (char=? (string-ref input-str ii) #\space)) (+ ii 1) ii)
                                 ) ;skip-ii
                                ) ;
                             (let-values (((val next-ii) (read-digits skip-ii 1 2)))
                               (set! day val)
                               (parse-loop (+ next-fi 1) next-ii)
                             ) ;let-values
                           ) ;let
                          ) ;
                          ((#\H)
                           (let-values (((val next-ii) (read-digits ii 1 2)))
                             (set! hour val)
                             (parse-loop (+ next-fi 1) next-ii)
                           ) ;let-values
                          ) ;
                          ((#\I)
                           (let-values (((val next-ii) (read-digits ii 1 2)))
                             (set! hour12 val)
                             (parse-loop (+ next-fi 1) next-ii)
                           ) ;let-values
                          ) ;
                          ((#\M)
                           (let-values (((val next-ii) (read-digits ii 1 2)))
                             (set! minute val)
                             (parse-loop (+ next-fi 1) next-ii)
                           ) ;let-values
                          ) ;
                          ((#\S)
                           (let-values (((val next-ii) (read-digits ii 1 2)))
                             (set! second val)
                             (parse-loop (+ next-fi 1) next-ii)
                           ) ;let-values
                          ) ;
                          ((#\f)
                           (let-values (((val next-ii) (read-microsecond ii)))
                             (set! microsecond val)
                             (parse-loop (+ next-fi 1) next-ii)
                           ) ;let-values
                          ) ;
                          ((#\p #\P)
                           (cond ((string-ci-prefix? input-str ii "am")
                                  (set! ampm 'am)
                                  (parse-loop (+ next-fi 1) (+ ii 2))
                                 ) ;
                                 ((string-ci-prefix? input-str ii "pm")
                                  (set! ampm 'pm)
                                  (parse-loop (+ next-fi 1) (+ ii 2))
                                 ) ;
                                 (else (error 'value-error "date-time-parse: expected AM or PM" input-str))
                           ) ;cond
                          ) ;
                          ((#\b #\h)
                           (let-values (((idx next-ii) (match-vector-name ii ABBR-MONTH 1)))
                             (set! month idx)
                             (parse-loop (+ next-fi 1) next-ii)
                           ) ;let-values
                          ) ;
                          ((#\B)
                           (let-values (((idx next-ii) (match-vector-name ii LONG-MONTH 1)))
                             (set! month idx)
                             (parse-loop (+ next-fi 1) next-ii)
                           ) ;let-values
                          ) ;
                          ((#\a)
                           (let-values (((idx next-ii) (match-vector-name ii ABBR-WEEKDAY 0)))
                             (parse-loop (+ next-fi 1) next-ii)
                           ) ;let-values
                          ) ;
                          ((#\A)
                           (let-values (((idx next-ii) (match-vector-name ii LONG-WEEKDAY 0)))
                             (parse-loop (+ next-fi 1) next-ii)
                           ) ;let-values
                          ) ;
                          ((#\j)
                           (let-values (((val next-ii) (read-digits ii 1 3)))
                             (set! year-day val)
                             (parse-loop (+ next-fi 1) next-ii)
                           ) ;let-values
                          ) ;
                          ((#\w #\u)
                           (let-values (((val next-ii) (read-digits ii 1 1)))
                             (parse-loop (+ next-fi 1) next-ii)
                           ) ;let-values
                          ) ;
                          ((#\z) (let ((next-ii (parse-tz ii))) (parse-loop (+ next-fi 1) next-ii)))
                          ((#\F)
                           ;; 展开 %Y-%m-%d
                           (let* ((sub-fmt "%Y-%m-%d") (remain-fmt (substring fmt (+ next-fi 1) fmt-len)))
                             (set! fmt (string-append (substring fmt 0 fi) sub-fmt remain-fmt))
                             (set! fmt-len (string-length fmt))
                             (parse-loop fi ii)
                           ) ;let*
                          ) ;
                          ((#\T)
                           ;; 展开 %H:%M:%S
                           (let* ((sub-fmt "%H:%M:%S") (remain-fmt (substring fmt (+ next-fi 1) fmt-len)))
                             (set! fmt (string-append (substring fmt 0 fi) sub-fmt remain-fmt))
                             (set! fmt-len (string-length fmt))
                             (parse-loop fi ii)
                           ) ;let*
                          ) ;
                          ((#\R)
                           ;; 展开 %H:%M
                           (let* ((sub-fmt "%H:%M") (remain-fmt (substring fmt (+ next-fi 1) fmt-len)))
                             (set! fmt (string-append (substring fmt 0 fi) sub-fmt remain-fmt))
                             (set! fmt-len (string-length fmt))
                             (parse-loop fi ii)
                           ) ;let*
                          ) ;
                          ((#\%)
                           (let ((next-ii (expect-char ii #\%)))
                             (parse-loop (+ next-fi 1) next-ii)
                           ) ;let
                          ) ;
                          (else (error 'value-error "date-time-parse: unknown directive in fmt" ch))
                         ) ;case
                       ) ;let
                     ) ;if
                   ) ;let
                  ) ;
                  (else
                    (let ((next-ii (expect-char ii (string-ref fmt fi))))
                      (parse-loop (+ fi 1) next-ii)
                    ) ;let
                  ) ;else
            ) ;cond
          ) ;let

          ;; 解析后推导与归一化
          ;; 1. 2位年份回算 (00-68 -> 2000-2068, 69-99 -> 1969-1999)
          (when (and (not year) year-2digit)
            (set! year (if (<= year-2digit 68) (+ 2000 year-2digit) (+ 1900 year-2digit)))
          ) ;when

          (set! year (or year 1970))

          ;; 2. 12 小时制回算
          (when hour12
            (let ((h (modulo hour12 12)))
              (set! hour (if (eq? ampm 'pm) (+ h 12) h))
            ) ;let
          ) ;when

          ;; 3. 年内天数 %j 回算月和日
          (when (and year-day (or (not month) (not day)))
            (let* ((jan1-rd (date->rata-die (make-date year 1 1)))
                   (target-rd (+ jan1-rd (- year-day 1)))
                   (d (rata-die->date target-rd))
                  ) ;
              (set! month (date-month d))
              (set! day (date-day d))
            ) ;let*
          ) ;when

          (set! month (or month 1))
          (set! day (or day 1))
          (set! hour (or hour 0))
          (set! minute (or minute 0))
          (set! second (or second 0))

          ;; 4. 校验范围
          (unless (and (integer? year))
            (error 'value-error "date-time-parse: invalid year" year)
          ) ;unless
          (unless (and (integer? month) (>= month 1) (<= month 12))
            (error 'value-error "date-time-parse: invalid month" month)
          ) ;unless
          (let ((max-d (days-in-month year month)))
            (unless (and (integer? day) (>= day 1) (<= day max-d))
              (error 'value-error "date-time-parse: invalid day" day)
            ) ;unless
          ) ;let
          (unless (and (integer? hour) (>= hour 0) (<= hour 23))
            (error 'value-error "date-time-parse: invalid hour" hour)
          ) ;unless
          (unless (and (integer? minute) (>= minute 0) (<= minute 59))
            (error 'value-error "date-time-parse: invalid minute" minute)
          ) ;unless
          (unless (and (>= second 0) (< second 61))
            (error 'value-error "date-time-parse: invalid second" second)
          ) ;unless

          ;; 5. 组合微秒
          (let* ((final-sec (if microsecond (+ second (/ microsecond 1000000)) second))
                 (final-tz (if tz-delta (utc-offset-timezone tz-delta) default-tz))
                ) ;
            (make-timestamp year month day hour minute final-sec final-tz)
          ) ;let*
        ) ;let
      ) ;let*
    ) ;define
  ) ;begin

) ;define-library
