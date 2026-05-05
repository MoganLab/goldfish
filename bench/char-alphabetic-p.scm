;; char-alphabetic? 性能测试

(import (scheme base)
  (scheme char)
  (liii timeit)
)

(define iterations 100000)

(define (bench-ascii-letter)
  (timeit (lambda () (char-alphabetic? #\a)) '() iterations)
)

(define (bench-ascii-digit)
  (timeit (lambda () (char-alphabetic? #\5)) '() iterations)
)

(define (bench-ascii-space)
  (timeit (lambda () (char-alphabetic? #\space)) '() iterations)
)

(define (bench-unicode-cjk)
  (timeit (lambda () (char-alphabetic? #\x4E2D)) '() iterations)
)

(define (bench-unicode-symbol)
  (timeit (lambda () (char-alphabetic? #\x2600)) '() iterations)
)

(define (bench-beyond-max)
  (timeit (lambda () (char-alphabetic? #\x40000)) '() iterations)
)

(display "=== char-alphabetic? 性能测试 ===\n")
(display "迭代次数: ")
(display iterations)
(display "\n\n")

(display "ASCII 字母 (#\\a) -> 命中:\n")
(display "  ")
(display (bench-ascii-letter))
(display " 秒\n\n")

(display "ASCII 数字 (#\\5) -> 未命中:\n")
(display "  ")
(display (bench-ascii-digit))
(display " 秒\n\n")

(display "ASCII 空格 -> 未命中:\n")
(display "  ")
(display (bench-ascii-space))
(display " 秒\n\n")

(display "Unicode CJK (#\\x4E2D) -> 命中:\n")
(display "  ")
(display (bench-unicode-cjk))
(display " 秒\n\n")

(display "Unicode 符号 (#\\x2600) -> 未命中:\n")
(display "  ")
(display (bench-unicode-symbol))
(display " 秒\n\n")

(display "超出上届 (#\\x40000) -> 快速拒绝:\n")
(display "  ")
(display (bench-beyond-max))
(display " 秒\n")
