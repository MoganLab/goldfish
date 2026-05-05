;; char-foldcase 性能测试

(import (scheme base)
  (scheme char)
  (liii timeit)
)

(define iterations 100000)

(define (bench-ascii-upper)
  (timeit (lambda () (char-foldcase #\A)) '() iterations)
)

(define (bench-ascii-lower)
  (timeit (lambda () (char-foldcase #\a)) '() iterations)
)

(define (bench-ascii-digit)
  (timeit (lambda () (char-foldcase #\5)) '() iterations)
)

(define (bench-ascii-space)
  (timeit (lambda () (char-foldcase #\space)) '() iterations)
)

(define (bench-special-383)
  (timeit (lambda () (char-foldcase #\x17F)) '() iterations)
)

(define (bench-special-304)
  (timeit (lambda () (char-foldcase #\x130)) '() iterations)
)

(display "=== char-foldcase 性能测试 ===\n")
(display "迭代次数: ")
(display iterations)
(display "\n\n")

(display "ASCII 大写 (#\\A) -> 回退 char-downcase, 命中前部分支:\n")
(display "  ")
(display (bench-ascii-upper))
(display " 秒\n\n")

(display "ASCII 小写 (#\\a) -> 回退 char-downcase, 未命中:\n")
(display "  ")
(display (bench-ascii-lower))
(display " 秒\n\n")

(display "ASCII 数字 (#\\5) -> 回退 char-downcase, 未命中:\n")
(display "  ")
(display (bench-ascii-digit))
(display " 秒\n\n")

(display "ASCII 空格 -> 回退 char-downcase, 未命中:\n")
(display "  ")
(display (bench-ascii-space))
(display " 秒\n\n")

(display "特殊码点 #\\x17F -> 命中 foldcase 自身分支:\n")
(display "  ")
(display (bench-special-383))
(display " 秒\n\n")

(display "特殊码点 #\\x130 -> 命中 foldcase 自身分支:\n")
(display "  ")
(display (bench-special-304))
(display " 秒\n")
