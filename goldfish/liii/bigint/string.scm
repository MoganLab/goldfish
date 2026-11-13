;;; liii/bigint/string.scm - bigint 字符串与整数转换
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

(define-library (liii bigint string)
  (import (scheme base) (liii error) (liii bigint record))

  (export ensure-bigint char->digit digit->char string->bigint integer->bigint
    bigint->string
  ) ;export

  (begin
    (define (ensure-bigint who obj)
      (if (bigint? obj)
        obj
        (type-error (string-append who " in (liii bigint string): not a bigint") obj)
      ) ;if
    ) ;define

    (define (ensure-radix who radix)
      (if (not (integer? radix))
        (type-error (string-append who " in (liii bigint string): radix must be an integer")
          radix
        ) ;type-error
        (if (and (<= 2 radix) (<= radix 36))
          radix
          (value-error (string-append who " in (liii bigint string): radix must be between 2 and 36")
            radix
          ) ;value-error
        ) ;if
      ) ;if
    ) ;define

    (define (char->digit c)
      (cond ((and (char>=? c #\0) (char<=? c #\9))
             (- (char->integer c) (char->integer #\0))
            ) ;
            ((and (char>=? c #\a) (char<=? c #\z))
             (+ 10 (- (char->integer c) (char->integer #\a)))
            ) ;
            ((and (char>=? c #\A) (char<=? c #\Z))
             (+ 10 (- (char->integer c) (char->integer #\A)))
            ) ;
            (else -1)
      ) ;cond
    ) ;define

    (define (digit->char digit)
      (if (< digit 10)
        (integer->char (+ (char->integer #\0) digit))
        (integer->char (+ (char->integer #\a) (- digit 10)))
      ) ;if
    ) ;define

    (define (add-mag-with-base a b base)
      (let loop
        ((a (reverse a)) (b (reverse b)) (carry 0) (result '()))
        (cond ((and (null? a) (null? b)) (if (= carry 0) result (cons carry result)))
              ((null? a)
               (let* ((sum (+ (car b) carry))
                      (digit (remainder sum base))
                      (next-carry (quotient sum base))
                     ) ;
                 (loop '() (cdr b) next-carry (cons digit result))
               ) ;let*
              ) ;
              ((null? b)
               (let* ((sum (+ (car a) carry))
                      (digit (remainder sum base))
                      (next-carry (quotient sum base))
                     ) ;
                 (loop (cdr a) '() next-carry (cons digit result))
               ) ;let*
              ) ;
              (else
                (let* ((sum (+ (car a) (car b) carry))
                       (digit (remainder sum base))
                       (next-carry (quotient sum base))
                      ) ;
                  (loop (cdr a) (cdr b) next-carry (cons digit result))
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (mul-mag-small-with-base mag multiplier base)
      (if (or (null? mag) (= multiplier 0))
        '()
        (let loop
          ((digits (reverse mag)) (carry 0) (result '()))
          (if (null? digits)
            (if (= carry 0) result (cons carry result))
            (let* ((product (+ (* (car digits) multiplier) carry))
                   (digit (remainder product base))
                   (next-carry (quotient product base))
                  ) ;
              (loop (cdr digits) next-carry (cons digit result))
            ) ;let*
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (div-mag-small-with-base mag divisor base)
      (let loop
        ((digits mag) (remainder-val 0) (result '()))
        (if (null? digits)
          (values (strip-leading-zeros (reverse result)) remainder-val)
          (let* ((current (+ (* remainder-val base) (car digits)))
                 (quot (quotient current divisor))
                 (rem-val (remainder current divisor))
                ) ;
            (loop (cdr digits) rem-val (cons quot result))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (mul-mag-small mag multiplier)
      (mul-mag-small-with-base mag multiplier BIGINT-BASE)
    ) ;define

    (define (add-mag-small mag digit)
      (add-mag-with-base mag (list digit) BIGINT-BASE)
    ) ;define

    (define (mag->string-with-radix mag radix)
      (if (null? mag)
        "0"
        (let loop
          ((current mag) (chars '()))
          (if (null? current)
            (list->string chars)
            (call-with-values (lambda () (div-mag-small-with-base current radix BIGINT-BASE))
              (lambda (quotient-val remainder-val)
                (loop quotient-val (cons (digit->char remainder-val) chars))
              ) ;lambda
            ) ;call-with-values
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (parse-radix-mag str start-index len radix)
      (let loop
        ((index start-index) (mag '()))
        (if (= index len)
          (strip-leading-zeros mag)
          (let* ((ch (string-ref str index)) (digit (char->digit ch)))
            (if (or (< digit 0) (>= digit radix))
              (value-error "string->bigint in (liii bigint string): invalid digit for radix"
                ch radix str
              ) ;value-error
              (let* ((scaled (mul-mag-small mag radix))
                     (next-mag (if (= digit 0) scaled (add-mag-small scaled digit)))
                    ) ;
                (loop (+ index 1) next-mag)
              ) ;let*
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (string->bigint str . maybe-radix)
      (let* ((radix (if (null? maybe-radix) 10 (ensure-radix "string->bigint" (car maybe-radix)))
             ) ;radix
             (len (if (string? str)
                    (string-length str)
                    (type-error "string->bigint in (liii bigint string): not a string" str)
                  ) ;if
             ) ;len
            ) ;
        (if (= len 0)
          (value-error "string->bigint in (liii bigint string): empty string" str)
          (let* ((first-char (string-ref str 0))
                 (has-sign (or (char=? first-char #\+) (char=? first-char #\-)))
                 (signum (if (char=? first-char #\-) -1 1))
                 (start-index (if has-sign 1 0))
                ) ;
            (if (= start-index len)
              (value-error "string->bigint in (liii bigint string): string contains only sign"
                str
              ) ;value-error
              (let ((normalized (parse-radix-mag str start-index len radix)))
                (if (null? normalized) (make-bigint 0 '()) (make-bigint signum normalized))
              ) ;let
            ) ;if
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (integer->bigint n)
      (cond ((bigint? n) n)
            ((integer? n) (string->bigint (number->string n)))
            (else (type-error "integer->bigint in (liii bigint string): not an integer" n))
      ) ;cond
    ) ;define

    (define (bigint->string n . maybe-radix)
      (let* ((radix (if (null? maybe-radix) 10 (ensure-radix "bigint->string" (car maybe-radix)))
             ) ;radix
             (value (ensure-bigint "bigint->string" n))
             (signum (bigint-signum value))
             (mag (bigint-mag value))
            ) ;
        (cond ((= signum 0) "0")
              ((= signum 1) (mag->string-with-radix mag radix))
              (else (string-append "-" (mag->string-with-radix mag radix)))
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
