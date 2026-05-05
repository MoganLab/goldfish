;; char-downcase 性能测试

(import (scheme base)
  (scheme char)
  (liii timeit)
)

(define iterations 100000)

(define (bench-ascii-upper)
  (timeit (lambda () (char-downcase #\A)) '() iterations)
)

(define (bench-ascii-lower)
  (timeit (lambda () (char-downcase #\a)) '() iterations)
)

(define (bench-ascii-digit)
  (timeit (lambda () (char-downcase #\5)) '() iterations)
)

(define (bench-ascii-space)
  (timeit (lambda () (char-downcase #\space)) '() iterations)
)

(define (bench-unicode-upper)
  (timeit (lambda () (char-downcase #\x391)) '() iterations)
)

(define (bench-unicode-lower)
  (timeit (lambda () (char-downcase #\x3B1)) '() iterations)
)

(display "=== char-downcase 性能测试 ===\n")
(display "迭代次数: ")
(display iterations)
(display "\n\n")

(display "ASCII 大写 (#\\A) -> 命中前部分支:\n")
(display "  ")
(display (bench-ascii-upper))
(display " 秒\n\n")

(display "ASCII 小写 (#\\a) -> 未命中:\n")
(display "  ")
(display (bench-ascii-lower))
(display " 秒\n\n")

(display "ASCII 数字 (#\\5) -> 未命中:\n")
(display "  ")
(display (bench-ascii-digit))
(display " 秒\n\n")

(display "ASCII 空格 -> 未命中:\n")
(display "  ")
(display (bench-ascii-space))
(display " 秒\n\n")

(display "Unicode 大写 (#\\x391) -> 命中中后部分支:\n")
(display "  ")
(display (bench-unicode-upper))
(display " 秒\n\n")

(display "Unicode 小写 (#\\x3B1) -> 未命中:\n")
(display "  ")
(display (bench-unicode-lower))
(display " 秒\n")
