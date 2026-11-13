;;; liii/bigint/division.scm - bigint 十进制除法算法
;;;
;;; Copyright (c) 2024-2026 Liii Network
;;; All Rights Reserved

(define-library (liii bigint division)
  (import (scheme base) (liii error) (liii bigint record) (liii bigint string))

  (export positive-divide-and-remainder
    divide-decimal-strings
    strip-leading-zero-chars
  ) ;export

  (begin
    (define (strip-leading-zero-chars str)
      (let ((len (string-length str)))
        (let loop
          ((index 0))
          (if (>= index len)
            "0"
            (if (char=? (string-ref str index) #\0)
              (loop (+ index 1))
              (substring str index len)
            ) ;if
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (compare-decimal-strings a b)
      (let* ((a (strip-leading-zero-chars a))
             (b (strip-leading-zero-chars b))
             (len-a (string-length a))
             (len-b (string-length b))
            ) ;
        (cond ((< len-a len-b) -1)
              ((> len-a len-b) 1)
              (else
                (let loop
                  ((index 0))
                  (if (= index len-a)
                    0
                    (let ((ca (string-ref a index)) (cb (string-ref b index)))
                      (cond ((char<? ca cb) -1)
                            ((char>? ca cb) 1)
                            (else (loop (+ index 1)))
                      ) ;cond
                    ) ;let
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (subtract-decimal-strings a b)
      (let ((a-len (string-length a)) (b-len (string-length b)))
        (strip-leading-zero-chars
          (list->string
            (let loop
              ((i (- a-len 1)) (j (- b-len 1)) (borrow 0) (result '()))
              (if (< i 0)
                result
                (let* ((da (- (char->integer (string-ref a i)) (char->integer #\0)))
                       (db
                         (if (< j 0) 0 (- (char->integer (string-ref b j)) (char->integer #\0)))
                       ) ;db
                       (diff (- da db borrow))
                      ) ;
                  (if (< diff 0)
                    (loop (- i 1)
                      (- j 1)
                      1
                      (cons (integer->char (+ (char->integer #\0) (+ diff 10))) result)
                    ) ;loop
                    (loop (- i 1)
                      (- j 1)
                      0
                      (cons (integer->char (+ (char->integer #\0) diff)) result)
                    ) ;loop
                  ) ;if
                ) ;let*
              ) ;if
            ) ;let
          ) ;list->string
        ) ;strip-leading-zero-chars
      ) ;let
    ) ;define

    (define (divide-digit-step current divisor)
      (let loop
        ((count 0) (rem current))
        (if (< (compare-decimal-strings rem divisor) 0)
          (cons count rem)
          (loop (+ count 1) (subtract-decimal-strings rem divisor))
        ) ;if
      ) ;let
    ) ;define

    ;; 返回 (cons quotient-string remainder-string)。
    (define (divide-decimal-strings dividend divisor)
      (let ((dividend (strip-leading-zero-chars dividend))
            (divisor (strip-leading-zero-chars divisor))
           ) ;
        (cond ((string=? divisor "0") (error "bigint division by zero"))
              ((< (compare-decimal-strings dividend divisor) 0) (cons "0" dividend))
              (else
                (let loop
                  ((index 0) (remainder "0") (quotient-chars '()))
                  (if (= index (string-length dividend))
                    (cons (strip-leading-zero-chars (list->string (reverse quotient-chars)))
                      (strip-leading-zero-chars remainder)
                    ) ;cons
                    (let* ((digit-str (string (string-ref dividend index)))
                           (next-remainder (strip-leading-zero-chars (if (string=? remainder "0") digit-str (string-append remainder digit-str))
                                           ) ;strip-leading-zero-chars
                           ) ;next-remainder
                           (step (divide-digit-step next-remainder divisor))
                           (count (car step))
                           (new-rem (cdr step))
                           (ch (integer->char (+ (char->integer #\0) count)))
                          ) ;
                      (loop (+ index 1) new-rem (cons ch quotient-chars))
                    ) ;let*
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (bigint-abs-decimal-string n)
      (let ((text (bigint->string n)))
        (if (and (> (string-length text) 0) (char=? (string-ref text 0) #\-))
          (substring text 1 (string-length text))
          text
        ) ;if
      ) ;let
    ) ;define

    (define (bigint-from-nonnegative-decimal-string str)
      (string->bigint (strip-leading-zero-chars str))
    ) ;define

    (define (positive-divide-and-remainder a b)
      (let* ((result (divide-decimal-strings (bigint-abs-decimal-string a)
                       (bigint-abs-decimal-string b)
                     ) ;divide-decimal-strings
             ) ;result
             (quotient (bigint-from-nonnegative-decimal-string (car result)))
             (remainder (bigint-from-nonnegative-decimal-string (cdr result)))
            ) ;
        (values quotient remainder)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
