;;; liii/bigint/predicate.scm - bigint 谓词函数
;;;
;;; 负责 bigint 的各种谓词判断。
;;;
;;; Copyright (c) 2024 Liii Network
;;; All Rights Reserved

(define-library (liii bigint predicate)
  (import (scheme base) (liii error) (liii bigint record))

  (export bigint? bigint-zero? bigint-positive? bigint-negative? bigint-even?
    bigint-odd?
  ) ;export

  (begin
    (define (ensure-bigint who obj)
      (if (bigint? obj)
        obj
        (type-error (string-append who " in (liii bigint predicate): not a bigint") obj)
      ) ;if
    ) ;define

    ;;; ---------- 基本谓词 ----------
    ;; bigint? 已由 bigint-record 提供，这里通过重新导出使接口完整

    (define (bigint-zero? n)
      (= (bigint-signum (ensure-bigint "bigint-zero?" n)) 0)
    ) ;define

    (define (bigint-positive? n)
      (= (bigint-signum (ensure-bigint "bigint-positive?" n)) 1)
    ) ;define

    (define (bigint-negative? n)
      (= (bigint-signum (ensure-bigint "bigint-negative?" n)) -1)
    ) ;define

    ;;; ---------- 奇偶判断 ----------
    (define (bigint-even? n)
      (let ((value (ensure-bigint "bigint-even?" n)))
        (let ((mag (bigint-mag value)))
          (or (null? mag) (let ((lsb (car (reverse mag)))) (even? lsb)))
        ) ;let
      ) ;let
    ) ;define

    (define (bigint-odd? n)
      (not (bigint-even? n))
    ) ;define

  ) ;begin
) ;define-library
