;; char-numeric? vs ascii-numeric? 性能对比测试
;; char-numeric? 现基于 s7 hash-table 实现

(import (scheme base)
  (scheme char)
  (scheme time)
  (liii ascii)
  (liii timeit)
)

(define iterations 100000)

(define (bench-char-numeric-ascii-digit)
  (timeit (lambda () (char-numeric? #\5)) '() iterations)
)

(define (bench-ascii-numeric-ascii-digit)
  (timeit (lambda () (ascii-numeric? #\5)) '() iterations)
)

(define (bench-char-numeric-ascii-letter)
  (timeit (lambda () (char-numeric? #\a)) '() iterations)
)

(define (bench-ascii-numeric-ascii-letter)
  (timeit (lambda () (ascii-numeric? #\a)) '() iterations)
)

(define (bench-char-numeric-unicode-digit)
  (timeit (lambda () (char-numeric? #\x660)) '() iterations)
)

(define (bench-ascii-numeric-unicode-digit)
  (timeit (lambda () (ascii-numeric? #\x660)) '() iterations)
)

(display "=== char-numeric? (hash-table) vs ascii-numeric? 性能测试 ===\n")
(display "迭代次数: ")
(display iterations)
(display "\n\n")

(display "ASCII 数字 (#\\5):\n")
(display "  char-numeric?   : ")
(display (bench-char-numeric-ascii-digit))
(display " 秒\n")
(display "  ascii-numeric?  : ")
(display (bench-ascii-numeric-ascii-digit))
(display " 秒\n\n")

(display "ASCII 字母 (#\\a):\n")
(display "  char-numeric?   : ")
(display (bench-char-numeric-ascii-letter))
(display " 秒\n")
(display "  ascii-numeric?  : ")
(display (bench-ascii-numeric-ascii-letter))
(display " 秒\n\n")

(display "Unicode 数字 (#\\x660):\n")
(display "  char-numeric?   : ")
(display (bench-char-numeric-unicode-digit))
(display " 秒\n")
(display "  ascii-numeric?  : ")
(display (bench-ascii-numeric-unicode-digit))
(display " 秒\n\n")
