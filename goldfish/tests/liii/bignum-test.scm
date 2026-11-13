;; (liii bignum) 单元测试
;; 验证任意精度整数数值塔的溢出提升、回退降级、比较、谓词与转换。

(import (liii check)
        (liii bignum)
) ;import

(check-set-mode! 'report-failed)

;;; 溢出提升与自动降级
(define big1 (+ 9223372036854775807 1))
(define big2 (- -9223372036854775808 1))
(define big3 (expt 10 30))

(check (bignum? big1) => #t)
(check (bignum? big2) => #t)
(check (bignum? big3) => #t)
(check (bignum? 42) => #f)
(check (bignum? (- big1 1)) => #f)
(check (- big1 1) => 9223372036854775807)
(check (+ big2 1) => -9223372036854775808)
(check (- -9223372036854775808) => big1)
(check (abs -9223372036854775808) => big1)

;;; 四则与幂运算
(check (number->string (+ big1 big1)) => "18446744073709551616")
(check (number->string (* big1 2)) => "18446744073709551616")
(check (/ big1 2) => 4611686018427387904)
(check (/ (* big1 3) (* big1 2)) => 3/2)
(check (number->string (expt 2 100)) => "1267650600228229401496703205376")
(check (quotient big3 (expt 10 29)) => 10)
(check (remainder (+ big3 7) 10) => 7)
(check (modulo (- (+ big3 7)) 10) => 3)
(check (gcd (expt 2 80) (* 3 (expt 2 70))) => (expt 2 70))
(check (lcm (expt 2 70) (* 3 (expt 2 70))) => (* 3 (expt 2 70)))

;;; 谓词与比较
(check (number? big3) => #t)
(check (complex? big3) => #t)
(check (real? big3) => #t)
(check (rational? big3) => #t)
(check (integer? big3) => #t)
(check (exact? big3) => #t)
(check (inexact? big3) => #f)
(check (exact-integer? big3) => #t)
(check (zero? big3) => #f)
(check (positive? big3) => #t)
(check (negative? big2) => #t)
(check (even? big1) => #t)
(check (odd? (+ big1 1)) => #t)

(check (= big1 (+ 9223372036854775807 1)) => #t)
(check (< big2 0 9223372036854775807 big1 big3) => #t)
(check (> big3 big1 9223372036854775807 0 big2) => #t)
(check (<= big1 big1 big3) => #t)
(check (>= big3 big1 big1) => #t)
(check (max 1 big1 big3) => big3)
(check (min big2 0 big1) => big2)
(check (eqv? big1 (+ 9223372036854775807 1)) => #t)

;;; 转换
(check (number->string (expt 2 64) 16) => "10000000000000000")
(check (exact big3) => big3)
(check (inexact? (inexact big3)) => #t)

(check-report)
