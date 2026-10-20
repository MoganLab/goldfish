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

(define-library (liii date-time parse)
  (import (scheme base)
    (liii date-time date)
    (liii date-time timezone)
    (liii date-time timestamp)
    (liii date-time gregorian)
    (liii date-time internal format-util)
  ) ;import
  (export date-time-parse)
  (begin

    (define (date-time-parse input-str . opts)
      (let* ((fmt (if (null? opts) DEFAULT-FMT (car opts)))
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

          ;; 将位置 fi 处的组合指令（占 2 字符）文本展开为 sub-fmt 后重新扫描
          (define (expand-composite fi ii sub-fmt)
            (set! fmt
              (string-append (substring fmt 0 fi) sub-fmt (substring fmt (+ fi 2) fmt-len))
            ) ;set!
            (set! fmt-len (string-length fmt))
            (parse-loop fi ii)
          ) ;define

          (define (skip-space ii)
            (if (and (< ii in-len) (char=? (string-ref input-str ii) #\space)) (+ ii 1) ii)
          ) ;define

          ;; 匹配并跳过星期/月份名称（解析结果不使用），返回新输入位置
          (define (skip-name ii vec start-idx)
            (let-values (((idx nii) (match-vector-name input-str ii vec start-idx)))
              nii
            ) ;let-values
          ) ;define

          ;; 解析时区偏移并记录，然后从 next-fi 处继续扫描
          (define (parse-zone next-fi ii)
            (let-values (((delta nii) (parse-tz-offset input-str ii)))
              (set! tz-delta delta)
              (parse-loop next-fi nii)
            ) ;let-values
          ) ;define

          ;; 处理 '%' 后的单个指令字符；fi 为 '%' 位置，next-fi 为指令字符位置
          (define (parse-directive ch fi next-fi ii)
            (case ch
             ((#\:)
              (let ((after-colon (+ next-fi 1)))
                (if (and (< after-colon fmt-len) (char=? (string-ref fmt after-colon) #\z))
                  (parse-zone (+ after-colon 1) ii)
                  (error 'value-error "date-time-parse: invalid directive %:" fmt)
                ) ;if
              ) ;let
             ) ;
             ((#\Y)
              (let-values (((val nii) (read-digits input-str ii 1 4)))
                (set! year val)
                (parse-loop (+ next-fi 1) nii)
              ) ;let-values
             ) ;
             ((#\y)
              (let-values (((val nii) (read-fixed-digits input-str ii 2)))
                (set! year-2digit val)
                (parse-loop (+ next-fi 1) nii)
              ) ;let-values
             ) ;
             ((#\m)
              (let-values (((val nii) (read-digits input-str ii 1 2)))
                (set! month val)
                (parse-loop (+ next-fi 1) nii)
              ) ;let-values
             ) ;
             ((#\d #\e)
              (let-values (((val nii) (read-digits input-str (skip-space ii) 1 2)))
                (set! day val)
                (parse-loop (+ next-fi 1) nii)
              ) ;let-values
             ) ;
             ((#\H)
              (let-values (((val nii) (read-digits input-str ii 1 2)))
                (set! hour val)
                (parse-loop (+ next-fi 1) nii)
              ) ;let-values
             ) ;
             ((#\I)
              (let-values (((val nii) (read-digits input-str ii 1 2)))
                (set! hour12 val)
                (parse-loop (+ next-fi 1) nii)
              ) ;let-values
             ) ;
             ((#\M)
              (let-values (((val nii) (read-digits input-str ii 1 2)))
                (set! minute val)
                (parse-loop (+ next-fi 1) nii)
              ) ;let-values
             ) ;
             ((#\S)
              (let-values (((val nii) (read-digits input-str ii 1 2)))
                (set! second val)
                (parse-loop (+ next-fi 1) nii)
              ) ;let-values
             ) ;
             ((#\f)
              (let-values (((val nii) (read-microsecond input-str ii)))
                (set! microsecond val)
                (parse-loop (+ next-fi 1) nii)
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
              (let-values (((idx nii) (match-vector-name input-str ii ABBR-MONTH 1)))
                (set! month idx)
                (parse-loop (+ next-fi 1) nii)
              ) ;let-values
             ) ;
             ((#\B)
              (let-values (((idx nii) (match-vector-name input-str ii LONG-MONTH 1)))
                (set! month idx)
                (parse-loop (+ next-fi 1) nii)
              ) ;let-values
             ) ;
             ((#\a) (parse-loop (+ next-fi 1) (skip-name ii ABBR-WEEKDAY 0)))
             ((#\A) (parse-loop (+ next-fi 1) (skip-name ii LONG-WEEKDAY 0)))
             ((#\j)
              (let-values (((val nii) (read-digits input-str ii 1 3)))
                (set! year-day val)
                (parse-loop (+ next-fi 1) nii)
              ) ;let-values
             ) ;
             ((#\w #\u)
              (let-values (((val nii) (read-digits input-str ii 1 1)))
                (parse-loop (+ next-fi 1) nii)
              ) ;let-values
             ) ;
             ((#\z) (parse-zone (+ next-fi 1) ii))
             ((#\F) (expand-composite fi ii "%Y-%m-%d"))
             ;; 展开 %Y-%m-%d
             ((#\T) (expand-composite fi ii "%H:%M:%S"))
             ;; 展开 %H:%M:%S
             ((#\R) (expand-composite fi ii "%H:%M"))
             ;; 展开 %H:%M
             ((#\%) (parse-loop (+ next-fi 1) (expect-char input-str fmt ii #\%)))
             (else (error 'value-error "date-time-parse: unknown directive in fmt" ch))
            ) ;case
          ) ;define

          ;; 处理位置 fi 处的 '%'
          (define (parse-percent fi ii)
            (let ((next-fi (+ fi 1)))
              (if (>= next-fi fmt-len)
                (error 'value-error "date-time-parse: unfinished directive in fmt" fmt)
                (parse-directive (string-ref fmt next-fi) fi next-fi ii)
              ) ;if
            ) ;let
          ) ;define

          (define (parse-loop fi ii)
            (cond ((>= fi fmt-len)
                   (if (< ii in-len)
                     (error 'value-error "date-time-parse: extra characters in input" input-str)
                     #t
                   ) ;if
                  ) ;
                  ((char=? (string-ref fmt fi) #\%) (parse-percent fi ii))
                  (else (parse-loop (+ fi 1) (expect-char input-str fmt ii (string-ref fmt fi))))
            ) ;cond
          ) ;define

          ;; 遍历 fmt
          (parse-loop 0 0)

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
