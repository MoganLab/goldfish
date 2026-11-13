;;; liii/decimal/string.scm - decimal 字符串解析与格式化
;;;
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

(define-library (liii decimal string)
  (import (scheme base) (liii error) (liii bigint) (liii decimal record))

  (export ensure-decimal string->decimal decimal->string decimal->plain-string
    decimal->engineering-string decimal->scientific-string
  ) ;export

  (begin
    (define (ensure-decimal who obj)
      (if (decimal? obj)
        obj
        (type-error (string-append who " in (liii decimal string): not a decimal") obj)
      ) ;if
    ) ;define

    (define (count-fraction-digits text)
      (let ((len (string-length text)))
        (let loop
          ((index 0) (seen-dot? #f) (count 0))
          (if (= index len)
            count
            (let ((ch (string-ref text index)))
              (cond
               ((char=? ch #\.) (if seen-dot? -1 (loop (+ index 1) #t count)))
               (seen-dot? (loop (+ index 1) #t (+ count 1)))
               (else (loop (+ index 1) #f count))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (remove-decimal-point text)
      (let ((len (string-length text)))
        (let loop
          ((index 0) (chars '()))
          (if (= index len)
            (list->string (reverse chars))
            (let ((ch (string-ref text index)))
              (if (char=? ch #\.) (loop (+ index 1) chars) (loop (+ index 1) (cons ch chars)))
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (find-exponent-char str len)
      (let loop
        ((i 0))
        (if (>= i len)
          #f
          (let ((ch (string-ref str i)))
            (if (or (char=? ch #\e) (char=? ch #\E)) i (loop (+ i 1)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (parse-exponent exp-str)
      (let ((len (string-length exp-str)))
        (if (= len 0)
          (value-error "string->decimal in (liii decimal string): invalid exponent"
            exp-str
          ) ;value-error
          (let* ((first-ch (string-ref exp-str 0))
                 (has-sign (or (char=? first-ch #\+) (char=? first-ch #\-)))
                 (sign (if (char=? first-ch #\-) -1 1))
                 (digits-start (if has-sign 1 0))
                ) ;
            (if (= digits-start len)
              (value-error "string->decimal in (liii decimal string): exponent has only sign"
                exp-str
              ) ;value-error
              (let loop
                ((i digits-start) (val 0))
                (if (= i len)
                  (* sign val)
                  (let ((ch (string-ref exp-str i)))
                    (if (and (char>=? ch #\0) (char<=? ch #\9))
                      (loop (+ i 1) (+ (* val 10) (- (char->integer ch) (char->integer #\0))))
                      (value-error "string->decimal in (liii decimal string): invalid digit in exponent"
                        ch
                        exp-str
                      ) ;value-error
                    ) ;if
                  ) ;let
                ) ;if
              ) ;let
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (all-digits? str start)
      (let ((len (string-length str)))
        (if (= start len)
          #f
          (let loop
            ((i start))
            (if (= i len)
              #t
              (let ((ch (string-ref str i)))
                (if (and (char>=? ch #\0) (char<=? ch #\9)) (loop (+ i 1)) #f)
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (parse-body-digits body sign-prefix exp-val)
      (let ((scale-base (count-fraction-digits body)))
        (when (< scale-base 0)
          (value-error "string->decimal in (liii decimal string): multiple decimal points"
            body
          ) ;value-error
        ) ;when
        (let* ((digits (remove-decimal-point body)) (digits-len (string-length digits)))
          (when (= digits-len 0)
            (value-error "string->decimal in (liii decimal string): no digits found" body)
          ) ;when
          (unless (all-digits? digits 0)
            (value-error "string->decimal in (liii decimal string): invalid characters"
              body
            ) ;value-error
          ) ;unless
          (let* ((norm-digits (if (string=? sign-prefix "-") (string-append "-" digits) digits))
                 (unscaled (string->bigint norm-digits))
                 (final-scale (- scale-base exp-val))
                ) ;
            (make-decimal-with-scale unscaled final-scale)
          ) ;let*
        ) ;let*
      ) ;let
    ) ;define

    (define (string->decimal str)
      (unless (string? str)
        (type-error "string->decimal in (liii decimal string): not a string" str)
      ) ;unless
      (let ((len (string-length str)))
        (when (= len 0)
          (value-error "string->decimal in (liii decimal string): empty string" str)
        ) ;when
        (let* ((e-idx (find-exponent-char str len))
               (coeff-str (if e-idx (substring str 0 e-idx) str))
               (exp-val
                 (if e-idx (parse-exponent (substring str (+ e-idx 1) len)) 0)
               ) ;exp-val
               (coeff-len (string-length coeff-str))
              ) ;
          (when (= coeff-len 0)
            (value-error "string->decimal in (liii decimal string): empty coefficient" str)
          ) ;when
          (let* ((first-ch (string-ref coeff-str 0))
                 (has-sign (or (char=? first-ch #\+) (char=? first-ch #\-)))
                 (sign-prefix (if (char=? first-ch #\-) "-" ""))
                 (body (if has-sign (substring coeff-str 1 coeff-len) coeff-str))
                 (body-len (string-length body))
                ) ;
            (when (= body-len 0)
              (value-error "string->decimal in (liii decimal string): coefficient has only sign"
                str
              ) ;value-error
            ) ;when
            (parse-body-digits body sign-prefix exp-val)
          ) ;let*
        ) ;let*
      ) ;let
    ) ;define

    (define (pad-left-with-zeroes text width)
      (let ((len (string-length text)))
        (if (>= len width) text (string-append (make-string (- width len) #\0) text))
      ) ;let
    ) ;define

    (define (decimal->string n)
      (let* ((value (ensure-decimal "decimal->string" n))
             (unscaled (decimal-unscaled value))
             (scale (decimal-scale value))
            ) ;
        (cond ((= scale 0) (bigint->string unscaled))
              ((> scale 0)
               (let* ((negative? (bigint-negative? unscaled))
                      (digits (bigint->string (bigint-abs unscaled)))
                      (padded (pad-left-with-zeroes digits (+ scale 1)))
                      (len (string-length padded))
                      (split (- len scale))
                      (int-part (substring padded 0 split))
                      (frac-part (substring padded split len))
                      (text (string-append int-part "." frac-part))
                     ) ;
                 (if negative? (string-append "-" text) text)
               ) ;let*
              ) ;
              (else
                (if (bigint-zero? unscaled)
                  "0"
                  (let* ((negative? (bigint-negative? unscaled))
                         (digits (bigint->string (bigint-abs unscaled)))
                         (text (string-append digits (make-string (- scale) #\0)))
                        ) ;
                    (if negative? (string-append "-" text) text)
                  ) ;let*
                ) ;if
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (decimal->plain-string n)
      (decimal->string n)
    ) ;define

    (define (decimal->scientific-string n)
      (let* ((value (ensure-decimal "decimal->scientific-string" n))
             (unscaled (decimal-unscaled value))
             (scale (decimal-scale value))
             (negative? (bigint-negative? unscaled))
             (abs-unscaled (bigint-abs unscaled))
            ) ;
        (if (bigint-zero? unscaled)
          "0E+0"
          (let* ((digits (bigint->string abs-unscaled))
                 (digit-count (string-length digits))
                 (exponent (- digit-count scale 1))
                 (exp-str (number->string (abs exponent)))
                ) ;
            (let ((mantissa (if (> digit-count 1)
                              (string-append (substring digits 0 1) "." (substring digits 1 digit-count))
                              digits
                            ) ;if
                  ) ;mantissa
                  (sign (if negative? "-" ""))
                  (exp-sign (if (>= exponent 0) "E+" "E-"))
                 ) ;
              (string-append sign mantissa exp-sign exp-str)
            ) ;let
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (decimal->engineering-string n)
      (let* ((value (ensure-decimal "decimal->engineering-string" n))
             (unscaled (decimal-unscaled value))
             (scale (decimal-scale value))
             (negative? (bigint-negative? unscaled))
             (abs-unscaled (bigint-abs unscaled))
            ) ;
        (if (bigint-zero? unscaled)
          "0E+0"
          (let* ((digits (bigint->string abs-unscaled))
                 (digit-count (string-length digits))
                 (raw-exponent (- digit-count scale 1))
                 (eng-exponent (* (floor (/ raw-exponent 3)) 3))
                 (exp-diff (- raw-exponent eng-exponent))
                 (int-digits (+ exp-diff 1))
                 (int-digits (max int-digits 1))
                 (padded-digits
                   (if (< digit-count int-digits)
                     (string-append digits (make-string (- int-digits digit-count) #\0))
                     digits
                   ) ;if
                 ) ;padded-digits
                 (padded-len (string-length padded-digits))
                ) ;
            (let* ((int-part (substring padded-digits 0 int-digits))
                   (raw-frac-part (if (< int-digits padded-len)
                                    (substring padded-digits int-digits padded-len)
                                    ""
                                  ) ;if
                   ) ;raw-frac-part
                   (frac-part
                     (let loop
                       ((i (- (string-length raw-frac-part) 1)))
                       (if (< i 0)
                         ""
                         (if (char=? (string-ref raw-frac-part i) #\0)
                           (loop (- i 1))
                           (substring raw-frac-part 0 (+ i 1))
                         ) ;if
                       ) ;if
                     ) ;let
                   ) ;frac-part
                   (sign (if negative? "-" ""))
                   (exp-sign (if (>= eng-exponent 0) "E+" "E"))
                   (exp-str (number->string eng-exponent))
                  ) ;
              (if (string=? frac-part "")
                (string-append sign int-part exp-sign exp-str)
                (string-append sign int-part "." frac-part exp-sign exp-str)
              ) ;if
            ) ;let*
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
