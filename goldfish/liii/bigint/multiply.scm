;;; liii/bigint/multiply.scm - bigint 乘法算法（Grade-school / Karatsuba / Toom-Cook）
;;;
;;; Copyright (c) 2024-2026 Liii Network
;;; All Rights Reserved

(define-library (liii bigint multiply)
  (import (scheme base) (liii list) (liii bigint record) (liii bigint mag))

  (export multiply-mag)

  (begin
    (define SMALL-BASE 65536)
    (define KARATSUBA-THRESHOLD 80)
    (define TOOM-COOK-THRESHOLD 240)

    ;; 将 32 位 mag 拆成 16 位 mag，以便安全地做乘法。
    (define (mag32->mag16 mag)
      (strip-leading-zeros
        (reverse
          (let loop
            ((digits mag) (result '()))
            (if (null? digits)
              result
              (let* ((digit (car digits))
                     (high (quotient digit SMALL-BASE))
                     (low (remainder digit SMALL-BASE))
                    ) ;
                (loop (cdr digits) (cons low (cons high result)))
              ) ;let*
            ) ;if
          ) ;let
        ) ;reverse
      ) ;strip-leading-zeros
    ) ;define

    (define (mag16->mag32 mag16)
      (if (null? mag16)
        '()
        (let ((digits (if (odd? (length mag16)) (cons 0 mag16) mag16)))
          (strip-leading-zeros
            (reverse
              (let loop
                ((digits digits) (result '()))
                (if (null? digits)
                  result
                  (let ((combined (+ (* (car digits) SMALL-BASE) (cadr digits))))
                    (loop (cddr digits) (cons combined result))
                  ) ;let
                ) ;if
              ) ;let
            ) ;reverse
          ) ;strip-leading-zeros
        ) ;let
      ) ;if
    ) ;define

    ;;; ---------- Karatsuba 乘法辅助函数 ----------

    (define (split-mag mag m)
      (let ((len (length mag)))
        (if (<= len m)
          (cons '() mag)
          (let ((split-point (- len m)))
            (cons (take mag split-point) (drop mag split-point))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (karatsuba-mag a b)
      (if (or (< (length a) KARATSUBA-THRESHOLD) (< (length b) KARATSUBA-THRESHOLD))
        (multiply-mag-grade-school a b)
        (let* ((n (max (length a) (length b)))
               (m (quotient n 2))
               (split-a (split-mag a m))
               (a1 (car split-a))
               (a0 (cdr split-a))
               (split-b (split-mag b m))
               (b1 (car split-b))
               (b0 (cdr split-b))
               (z0 (karatsuba-mag a0 b0))
               (z2 (karatsuba-mag a1 b1))
               (s1 (add-mag a1 a0))
               (s2 (add-mag b1 b0))
               (z1-temp (karatsuba-mag s1 s2))
               (z2-plus-z0 (add-mag z2 z0))
               (z1 (sub-mag z1-temp z2-plus-z0))
              ) ;
          (add-mag (add-mag (shift-mag z2 (* 2 m)) (shift-mag z1 m)) z0)
        ) ;let*
      ) ;if
    ) ;define

    ;;; ---------- Toom-Cook 3-way 乘法辅助函数 ----------

    (define (split-mag-3 mag m)
      (let ((len (length mag)))
        (cond ((<= len m) (list '() '() mag))
              ((<= len (* 2 m))
               (let* ((low (drop mag (- len m))) (mid (take mag (- len m))))
                 (list '() mid low)
               ) ;let*
              ) ;
              (else
                (let* ((low (drop mag (- len m)))
                       (rest (take mag (- len m)))
                       (mid (drop rest (- (length rest) m)))
                       (high (take rest (- (length rest) m)))
                      ) ;
                  (list high mid low)
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (mul-mag-small mag n)
      (if (or (null? mag) (= n 0))
        '()
        (let loop
          ((digits (reverse mag)) (carry 0) (result '()))
          (if (null? digits)
            (if (= carry 0) result (cons carry result))
            (let* ((product (+ (* (car digits) n) carry))
                   (digit (remainder product BIGINT-BASE))
                   (next-carry (quotient product BIGINT-BASE))
                  ) ;
              (loop (cdr digits) next-carry (cons digit result))
            ) ;let*
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (div-mag-by-2 mag)
      (let loop
        ((mag (reverse mag)) (carry 0) (result '()))
        (if (null? mag)
          (strip-leading-zeros (reverse result))
          (let* ((digit (+ (car mag) (* carry BIGINT-BASE)))
                 (new-digit (quotient digit 2))
                 (new-carry (remainder digit 2))
                ) ;
            (loop (cdr mag) new-carry (cons new-digit result))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (toom-cook-3-mag a b)
      (karatsuba-mag a b)
    ) ;define

    (define (multiply-mag-grade-school a b)
      (let ((a16 (mag32->mag16 a)) (b16 (mag32->mag16 b)))
        (mag16->mag32
          (let loop
            ((digits (reverse b16)) (shift 0) (result '()))
            (if (null? digits)
              result
              (let* ((partial (mul-mag-small-with-base a16 (car digits) SMALL-BASE))
                     (shifted (shift-mag partial shift))
                     (next-result (add-mag-with-base result shifted SMALL-BASE))
                    ) ;
                (loop (cdr digits) (+ shift 1) next-result)
              ) ;let*
            ) ;if
          ) ;let
        ) ;mag16->mag32
      ) ;let
    ) ;define

    (define (multiply-mag a b)
      (let ((len-a (length a)) (len-b (length b)))
        (cond ((or (null? a) (null? b)) '())
              ((or (< len-a KARATSUBA-THRESHOLD) (< len-b KARATSUBA-THRESHOLD))
               (multiply-mag-grade-school a b)
              ) ;
              ((or (< len-a TOOM-COOK-THRESHOLD) (< len-b TOOM-COOK-THRESHOLD))
               (karatsuba-mag a b)
              ) ;
              (else (toom-cook-3-mag a b))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
