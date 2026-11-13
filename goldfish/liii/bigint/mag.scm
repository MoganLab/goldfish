;;; liii/bigint/mag.scm - bigint 原始幅度向量运算
;;;
;;; Copyright (c) 2024-2026 Liii Network
;;; All Rights Reserved

(define-library (liii bigint mag)
  (import (scheme base) (liii error) (liii bigint record))

  (export add-mag sub-mag add-mag-with-base sub-mag-with-base compare-mag
    shift-mag mul-mag-small-with-base
  ) ;export

  (begin
    ;; 通用的同进制 mag 加法，输入与输出均为大端序。
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

    ;; 假定 a >= b >= 0。
    (define (sub-mag-with-base a b base)
      (let loop
        ((a (reverse a)) (b (reverse b)) (borrow 0) (result '()))
        (cond ((and (null? a) (null? b)) (strip-leading-zeros result))
              ((null? b)
               (let ((diff (- (car a) borrow)))
                 (if (< diff 0)
                   (loop (cdr a) '() 1 (cons (+ diff base) result))
                   (loop (cdr a) '() 0 (cons diff result))
                 ) ;if
               ) ;let
              ) ;
              (else
                (let ((diff (- (car a) (car b) borrow)))
                  (if (< diff 0)
                    (loop (cdr a) (cdr b) 1 (cons (+ diff base) result))
                    (loop (cdr a) (cdr b) 0 (cons diff result))
                  ) ;if
                ) ;let
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

    (define (compare-mag a b)
      (let ((len-a (length a)) (len-b (length b)))
        (cond ((< len-a len-b) -1)
              ((> len-a len-b) 1)
              (else
                (let loop
                  ((a a) (b b))
                  (cond ((null? a) 0)
                        ((< (car a) (car b)) -1)
                        ((> (car a) (car b)) 1)
                        (else (loop (cdr a) (cdr b)))
                  ) ;cond
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (add-mag a b)
      (add-mag-with-base a b BIGINT-BASE)
    ) ;define

    (define (sub-mag a b)
      (sub-mag-with-base a b BIGINT-BASE)
    ) ;define

    (define (shift-mag mag places)
      (if (null? mag) '() (append mag (make-list places 0)))
    ) ;define

  ) ;begin
) ;define-library
