;; decimal-hash-code
;; 计算 decimal 的哈希码，用于哈希表键。
;;
;; 语法
;; ----
;; (decimal-hash-code n)
;;
;; 参数
;; ----
;; n : decimal?
;;   decimal 值。
;;
;; 返回值
;; ----
;; integer?
;;   哈希码值。
;;
;; 说明
;; ----
;; 相同数值的 decimal 应该产生相同的哈希码。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)
;; 相同数值产生相同哈希码
(check (decimal-hash-code (string->decimal "123.456"))
       => (decimal-hash-code (string->decimal "123.456")))

;; 不同数值通常产生不同哈希码
(check (not (= (decimal-hash-code (string->decimal "123"))
               (decimal-hash-code (string->decimal "456")))) => #t)

;; 零值
(check (integer? (decimal-hash-code (string->decimal "0"))) => #t)

;; 负数
(check (integer? (decimal-hash-code (string->decimal "-123.456"))) => #t)

;; 结果应落在 fixnum 域内（BUG-8）
(let ((h (decimal-hash-code (string->decimal "12345678901234567890123456789.00"))))
  (check-true (and (integer? h) (>= h 0) (< h 2147483647)))
) ;let

;; decimal-equals? 相等则哈希码必然相等
(let ((a (string->decimal "123.45"))
      (b (string->decimal "123.45")))
  (check-true (decimal-equals? a b))
  (check (= (decimal-hash-code a) (decimal-hash-code b)) => #t)
) ;let

(check-report)
