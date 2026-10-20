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
    (liii string)
    (liii date-time date)
    (liii date-time clock-time)
    (liii date-time moment)
    (liii date-time timestamp)
    (liii date-time internal date-rd)
    (liii date-time internal format-util)
  ) ;import
  (export date-time-format)
  (begin

    (define (date-time-format obj . opt-fmt)
      (let ((fmt (if (null? opt-fmt) DEFAULT-FMT (car opt-fmt))))
        (unless (string? fmt)
          (error 'type-error "date-time-format: format-string must be a string" fmt)
        ) ;unless

        ;; 提取分量
        (let-values (((year month day hour minute second tz-offset tz-name iso-wday yearday)
                      (cond
                       ((timestamp? obj)
                        (let ((d (timestamp-date obj)))
                          (values (date-year d)
                            (date-month d)
                            (date-day d)
                            (timestamp-hour obj)
                            (timestamp-minute obj)
                            (timestamp-second obj)
                            (timestamp-timezone-offset obj)
                            (timestamp-timezone-abbreviation obj)
                            (date-iso-weekday d)
                            (days-since-year-start d)
                          ) ;values
                        ) ;let
                       ) ;
                       ((date? obj)
                        (values (date-year obj)
                          (date-month obj)
                          (date-day obj)
                          0
                          0
                          0
                          #f
                          #f
                          (date-iso-weekday obj)
                          (days-since-year-start obj)
                        ) ;values
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
                               (sod (moment-second-of-day obj))
                               (sod-floor (exact (floor sod)))
                               (h (quotient sod-floor 3600))
                               (m (quotient (modulo sod-floor 3600) 60))
                               (s (+ (modulo sod-floor 60) (- sod sod-floor)))
                              ) ;
                          (values (date-year d)
                            (date-month d)
                            (date-day d)
                            h
                            m
                            s
                            #f
                            #f
                            (date-iso-weekday d)
                            (days-since-year-start d)
                          ) ;values
                        ) ;let*
                       ) ;
                       (else (error 'type-error "date-time-format: unsupported date/time object" obj))
                      ) ;cond
                     ) ;
                    ) ;

          (let* ((out (open-output-string))
                 (wday (if (and iso-wday (= iso-wday 7)) 0 iso-wday))
                ) ;

            (define (pad0 x n)
              (display (pad-zero x n) out)
            ) ;define

            (define (format-directive ch)
              (case ch
               ((#\Y) (pad0 year 4))
               ((#\y) (pad0 (modulo year 100) 2))
               ((#\m) (pad0 month 2))
               ((#\d) (pad0 day 2))
               ((#\e) (display (string-pad (number->string day) 2 #\space) out))
               ((#\j) (pad0 (or yearday 0) 3))
               ((#\F) (format-scan "%Y-%m-%d"))
               ((#\H) (pad0 hour 2))
               ((#\I) (let ((h (modulo hour 12))) (pad0 (if (= h 0) 12 h) 2)))
               ((#\M) (pad0 minute 2))
               ((#\S) (pad0 (exact (floor second)) 2))
               ((#\f)
                (let* ((frac (- second (floor second))) (micros (exact (round (* frac 1000000)))))
                  (pad0 micros 6)
                ) ;let*
               ) ;
               ((#\p) (display (if (< hour 12) "AM" "PM") out))
               ((#\P) (display (if (< hour 12) "am" "pm") out))
               ((#\T) (format-scan "%H:%M:%S"))
               ((#\R) (format-scan "%H:%M"))
               ((#\a) (when wday (display (vector-ref ABBR-WEEKDAY wday) out)))
               ((#\A) (when wday (display (vector-ref LONG-WEEKDAY wday) out)))
               ((#\b #\h)
                (when (and (>= month 1) (<= month 12))
                  (display (vector-ref ABBR-MONTH month) out)
                ) ;when
               ) ;
               ((#\B)
                (when (and (>= month 1) (<= month 12))
                  (display (vector-ref LONG-MONTH month) out)
                ) ;when
               ) ;
               ((#\w) (when wday (display (number->string wday) out)))
               ((#\u) (when iso-wday (display (number->string iso-wday) out)))
               ((#\z) (when tz-offset (display (format-tz-offset tz-offset #f) out)))
               ((#\Z) (when tz-name (display tz-name out)))
               ((#\c) (format-scan "%a %b %e %H:%M:%S %Y"))
               ((#\x) (format-scan "%m/%d/%y"))
               ((#\X) (format-scan "%H:%M:%S"))
               ((#\%) (write-char #\% out))
               (else (error 'value-error "date-time-format: invalid directive" ch))
              ) ;case
            ) ;define

            ;; 处理 "%:z" 指令，返回其后的扫描位置
            (define (format-colon-z f next-i)
              (let ((after-colon (+ next-i 1)))
                (if (and (< after-colon (string-length f)) (char=? (string-ref f after-colon) #\z))
                  (begin
                    (when tz-offset
                      (display (format-tz-offset tz-offset #t) out)
                    ) ;when
                    (+ after-colon 1)
                  ) ;begin
                  (error 'value-error "date-time-format: invalid directive %:" f)
                ) ;if
              ) ;let
            ) ;define

            ;; 处理位置 i 处的 '%' 指令，返回下一个扫描位置
            (define (format-percent f i)
              (let ((next-i (+ i 1)))
                (if (>= next-i (string-length f))
                  (error 'value-error "date-time-format: unfinished directive at end of format" f)
                  (let ((ch (string-ref f next-i)))
                    (if (char=? ch #\:)
                      (format-colon-z f next-i)
                      (begin
                        (format-directive ch)
                        (+ next-i 1)
                      ) ;begin
                    ) ;if
                  ) ;let
                ) ;if
              ) ;let
            ) ;define

            ;; 扫描格式串；组合指令（%F %T %R %c %x %X）递归调用自身展开
            (define (format-scan f)
              (let ((f-len (string-length f)))
                (let loop
                  ((i 0))
                  (cond ((>= i f-len) #t)
                        ((char=? (string-ref f i) #\%) (loop (format-percent f i)))
                        (else (write-char (string-ref f i) out) (loop (+ i 1)))
                  ) ;cond
                ) ;let
              ) ;let
            ) ;define

            (format-scan fmt)
            (get-output-string out)
          ) ;let*
        ) ;let-values
      ) ;let
    ) ;define
  ) ;begin

) ;define-library
