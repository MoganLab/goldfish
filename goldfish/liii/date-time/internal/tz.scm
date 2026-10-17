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

(define-library (liii date-time internal tz)
  (import (scheme base))
  (export tz-offset tz-offset? tz-offset-sign-negative? tz-offset-hour
    tz-offset-minute tz-offset-second tz-month-week-day tz-month-week-day?
    tz-month-week-day:month tz-month-week-day:week tz-month-week-day:day
    tz-julian-without-leap-day tz-julian-without-leap-day?
    tz-julian-without-leap-day:date tz-julian-with-leap-day
    tz-julian-with-leap-day? tz-julian-with-leap-day:date tz-transition
    tz-transition? tz-transition-date tz-transition-time tz-string tz-string?
    tz-string-stdname tz-string-stdoffset tz-string-dstname tz-string-dstoffset
    tz-string-change-to-dst tz-string-change-to-std parse-tz
  ) ;export
  (begin

    (define-record-type <tz-offset>
      (tz-offset sign? hour minute second)
      tz-offset?
      (sign? tz-offset-sign-negative?)
      (hour tz-offset-hour)
      (minute tz-offset-minute)
      (second tz-offset-second)
    ) ;define-record-type

    (define-record-type <tz-month-week-day>
      (tz-month-week-day month week day)
      tz-month-week-day?
      (month tz-month-week-day:month)
      (week tz-month-week-day:week)
      (day tz-month-week-day:day)
    ) ;define-record-type

    (define-record-type <tz-julian-without-leap-day>
      (tz-julian-without-leap-day date)
      tz-julian-without-leap-day?
      (date tz-julian-without-leap-day:date)
    ) ;define-record-type

    (define-record-type <tz-julian-with-leap-day>
      (tz-julian-with-leap-day date)
      tz-julian-with-leap-day?
      (date tz-julian-with-leap-day:date)
    ) ;define-record-type

    (define-record-type <tz-transition>
      (tz-transition date time)
      tz-transition?
      (date tz-transition-date)
      (time tz-transition-time)
    ) ;define-record-type

    (define-record-type <tz-string>
      (tz-string stdname stdoffset dstname dstoffset change-to-dst change-to-std)
      tz-string?
      (stdname tz-string-stdname)
      (stdoffset tz-string-stdoffset)
      (dstname tz-string-dstname)
      (dstoffset tz-string-dstoffset)
      (change-to-dst tz-string-change-to-dst)
      (change-to-std tz-string-change-to-std)
    ) ;define-record-type

    (define (ASCII-alphabetic? ch)
      (or (char<=? #\A ch #\Z) (char<=? #\a ch #\z))
    ) ;define

    (define (ASCII-digit? ch)
      (char<=? #\0 ch #\9)
    ) ;define

    (define (ASCII-digit-value ch)
      (- (char->integer ch) (char->integer #\0))
    ) ;define

    (define (parse-tz input)
      (let ((str (cond ((string? input) input)
                       ((bytevector? input) (utf8->string input))
                       (else (error 'type-error "parse-tz: invalid input" input))
                 ) ;cond
            ) ;str
           ) ;
        (parse-tz-from-string str)
      ) ;let
    ) ;define

    (define (parse-tz-from-string str)
      (let ((len (string-length str)) (pos 0))

        (define (peek)
          (if (< pos len) (string-ref str pos) #f)
        ) ;define

        (define (next)
          (if (< pos len) (let ((ch (string-ref str pos))) (set! pos (+ pos 1)) ch) #f)
        ) ;define

        (define (parse-name)
          (let ((ch (peek)))
            (cond ((not ch) #f)
                  ((char=? ch #\<)
                   (next)
                   (let loop
                     ((acc '()))
                     (let ((c (next)))
                       (cond ((not c) (error 'type-error "EOF in quoted timezone name" str))
                             ((char=? c #\>) (list->string (reverse acc)))
                             (else (loop (cons c acc)))
                       ) ;cond
                     ) ;let
                   ) ;let
                  ) ;
                  ((ASCII-alphabetic? ch)
                   (let loop
                     ((acc '()))
                     (let ((c (peek)))
                       (if (and c (ASCII-alphabetic? c))
                         (begin
                           (next)
                           (loop (cons c acc))
                         ) ;begin
                         (list->string (reverse acc))
                       ) ;if
                     ) ;let
                   ) ;let
                  ) ;
                  (else #f)
            ) ;cond
          ) ;let
        ) ;define

        (define (parse-number)
          (let loop
            ((n 0) (count 0))
            (let ((ch (peek)))
              (if (and ch (ASCII-digit? ch))
                (begin
                  (next)
                  (loop (+ (* n 10) (ASCII-digit-value ch)) (+ count 1))
                ) ;begin
                (if (zero? count) #f n)
              ) ;if
            ) ;let
          ) ;let
        ) ;define

        (define (parse-offset)
          (let ((ch (peek)))
            (let ((is-neg
                    (cond ((and ch (char=? ch #\-)) (next) #t)
                          ((and ch (char=? ch #\+)) (next) #f)
                          (else #f)
                    ) ;cond
                  ) ;is-neg
                 ) ;
              (let ((hour (parse-number)))
                (unless hour
                  (error 'type-error "parse-offset: expected hour" str)
                ) ;unless
                (if (and (peek) (char=? (peek) #\:))
                  (begin
                    (next)
                    (let ((minute (parse-number)))
                      (unless minute
                        (error 'type-error "parse-offset: expected minute" str)
                      ) ;unless
                      (if (and (peek) (char=? (peek) #\:))
                        (begin
                          (next)
                          (let ((second (parse-number)))
                            (unless second
                              (error 'type-error "parse-offset: expected second" str)
                            ) ;unless
                            (tz-offset is-neg hour minute second)
                          ) ;let
                        ) ;begin
                        (tz-offset is-neg hour minute 0)
                      ) ;if
                    ) ;let
                  ) ;begin
                  (tz-offset is-neg hour 0 0)
                ) ;if
              ) ;let
            ) ;let
          ) ;let
        ) ;define

        (define (parse-offset-starts?)
          (let ((ch (peek)))
            (and ch (or (char=? ch #\+) (char=? ch #\-) (ASCII-digit? ch)))
          ) ;let
        ) ;define

        (define (parse-date)
          (let ((ch (peek)))
            (cond ((not ch) (error 'type-error "parse-date: unexpected EOF" str))
                  ((char=? ch #\J) (next) (tz-julian-without-leap-day (parse-number)))
                  ((char=? ch #\M)
                   (next)
                   (let* ((m (parse-number))
                          (_1
                            (unless (and (peek) (char=? (next) #\.))
                              (error 'type-error "expected ." str)
                            ) ;unless
                          ) ;_1
                          (w (parse-number))
                          (_2
                            (unless (and (peek) (char=? (next) #\.))
                              (error 'type-error "expected ." str)
                            ) ;unless
                          ) ;_2
                          (d (parse-number))
                         ) ;
                     (tz-month-week-day m w d)
                   ) ;let*
                  ) ;
                  (else (tz-julian-with-leap-day (parse-number)))
            ) ;cond
          ) ;let
        ) ;define

        (define (parse-offset-after-date)
          (if (and (peek) (char=? (peek) #\/))
            (begin
              (next)
              (parse-offset)
            ) ;begin
            (tz-offset #f 2 0 0)
          ) ;if
        ) ;define

        (define (parse-transition)
          (let* ((d (parse-date)) (t (parse-offset-after-date)))
            (tz-transition d t)
          ) ;let*
        ) ;define

        (let* ((stdname (parse-name)) (stdoffset (parse-offset)) (dstname (parse-name)))
          (if (not dstname)
            (tz-string stdname stdoffset #f #f #f #f)
            (let* ((dstoffset
                     (if (parse-offset-starts?)
                       (parse-offset)
                       (let ((is-neg (tz-offset-sign-negative? stdoffset))
                             (h (tz-offset-hour stdoffset))
                             (m (tz-offset-minute stdoffset))
                             (s (tz-offset-second stdoffset))
                            ) ;
                         (cond (is-neg (tz-offset #t (+ h 1) m s))
                               ((zero? h) (tz-offset #t 1 m s))
                               (else (tz-offset #f (- h 1) m s))
                         ) ;cond
                       ) ;let
                     ) ;if
                   ) ;dstoffset
                  ) ;
              (if (and (peek) (char=? (peek) #\,))
                (begin
                  (next)
                  (let* ((to-dst (parse-transition))
                         (_
                           (unless (and (peek) (char=? (next) #\,))
                             (error 'type-error "expected ," str)
                           ) ;unless
                         ) ;_
                         (to-std (parse-transition))
                        ) ;
                    (tz-string stdname stdoffset dstname dstoffset to-dst to-std)
                  ) ;let*
                ) ;begin
                (tz-string stdname stdoffset dstname dstoffset #f #f)
              ) ;if
            ) ;let*
          ) ;if
        ) ;let*
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
