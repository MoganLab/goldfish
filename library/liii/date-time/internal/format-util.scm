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

(define-library (liii date-time internal format-util)
  (import (scheme base) (scheme char) (liii string) (liii date-time time-delta))
  (export DEFAULT-FMT ABBR-WEEKDAY LONG-WEEKDAY ABBR-MONTH LONG-MONTH pad-zero
    string-ci-prefix? format-tz-offset expect-char read-digits read-fixed-digits
    read-microsecond match-vector-name parse-tz-offset
  ) ;export
  (begin

    (define DEFAULT-FMT "%Y-%m-%d %H:%M:%S")

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

    (define (pad-zero x n)
      (string-pad (number->string x) n #\0)
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
        (string-append sign-str
          (pad-zero abs-h 2)
          (if with-colon? ":" "")
          (pad-zero abs-m 2)
        ) ;string-append
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

    ;; ====================
    ;; date-time-parse 共享解析助手（均为纯函数，显式传入输入串）
    ;; ====================

    (define (expect-char input-str fmt pos ch)
      (if (and (< pos (string-length input-str)) (char=? (string-ref input-str pos) ch))
        (+ pos 1)
        (error 'value-error
          "date-time-parse: char mismatch"
          (list input-str fmt pos ch)
        ) ;error
      ) ;if
    ) ;define

    (define (read-digits input-str pos min-n max-n)
      (let ((in-len (string-length input-str)))
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
      ) ;let
    ) ;define

    (define (read-fixed-digits input-str pos n)
      (read-digits input-str pos n n)
    ) ;define

    (define (read-microsecond input-str pos)
      (let ((in-len (string-length input-str)))
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
      ) ;let
    ) ;define

    ;; 在 vec 中从 start-idx 起找与 input-str 在 pos 处大小写不敏感前缀匹配的最长名称，
    ;; 返回 (values 匹配下标 新位置)
    (define (match-vector-name input-str pos vec start-idx)
      (let ((len (vector-length vec)))
        (let loop
          ((i start-idx) (best-idx #f) (best-len 0))
          (if (= i len)
            (if best-idx
              (values best-idx (+ pos best-len))
              (error 'value-error "date-time-parse: name mismatch" (list input-str pos))
            ) ;if
            (let* ((name (vector-ref vec i)) (nlen (if (string? name) (string-length name) 0)))
              (if (and (> nlen 0) (string-ci-prefix? input-str pos name))
                (if (> nlen best-len) (loop (+ i 1) i nlen) (loop (+ i 1) best-idx best-len))
                (loop (+ i 1) best-idx best-len)
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 解析 ISO-8601 时区偏移（Z、+HHMM、+HH:MM），返回 (values time-delta 新位置)
    (define (parse-tz-offset input-str pos)
      (let ((in-len (string-length input-str)))
        (cond ((>= pos in-len)
               (error 'value-error "date-time-parse: expected timezone offset" input-str)
              ) ;
              ((char=? (string-ref input-str pos) #\Z)
               (values (make-time-delta 0 0 0 0 0 0 0) (+ pos 1))
              ) ;
              ((or (char=? (string-ref input-str pos) #\+)
                 (char=? (string-ref input-str pos) #\-)
               ) ;or
               (let* ((sign (if (char=? (string-ref input-str pos) #\-) -1 1)) (pos1 (+ pos 1)))
                 (let-values (((hh pos2) (read-fixed-digits input-str pos1 2)))
                   (let ((pos3
                           (if (and (< pos2 in-len) (char=? (string-ref input-str pos2) #\:))
                             (+ pos2 1)
                             pos2
                           ) ;if
                         ) ;pos3
                        ) ;
                     (let-values (((mm pos4) (read-fixed-digits input-str pos3 2)))
                       (values (time-delta+ (hours-delta (* sign hh)) (minutes-delta (* sign mm)))
                         pos4
                       ) ;values
                     ) ;let-values
                   ) ;let
                 ) ;let-values
               ) ;let*
              ) ;
              (else (error 'value-error "date-time-parse: invalid timezone format" input-str))
        ) ;cond
      ) ;let
    ) ;define
  ) ;begin

) ;define-library
