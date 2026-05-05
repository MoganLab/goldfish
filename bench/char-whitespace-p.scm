;; char-whitespace? 性能测试

(import (scheme base)
  (scheme char)
  (liii timeit)
)

(define iterations 100000)

(define (bench-space)
  (timeit (lambda () (char-whitespace? #\space)) '() iterations)
)

(define (bench-tab)
  (timeit (lambda () (char-whitespace? #\tab)) '() iterations)
)

(define (bench-letter)
  (timeit (lambda () (char-whitespace? #\a)) '() iterations)
)

(define (bench-digit)
  (timeit (lambda () (char-whitespace? #\5)) '() iterations)
)

(define (bench-nbsp)
  (timeit (lambda () (char-whitespace? #\xA0)) '() iterations)
)

(define (bench-ideographic-space)
  (timeit (lambda () (char-whitespace? #\x3000)) '() iterations)
)

(display "=== char-whitespace? 性能测试 ===\n")
(display "迭代次数: ")
(display iterations)
(display "\n\n")

(display "ASCII 空格 -> 命中:\n")
(display "  ")
(display (bench-space))
(display " 秒\n\n")

(display "ASCII tab -> 命中:\n")
(display "  ")
(display (bench-tab))
(display " 秒\n\n")

(display "ASCII 字母 (#\\a) -> 未命中:\n")
(display "  ")
(display (bench-letter))
(display " 秒\n\n")

(display "ASCII 数字 (#\\5) -> 未命中:\n")
(display "  ")
(display (bench-digit))
(display " 秒\n\n")

(display "Unicode nbsp (#\\xA0) -> 命中:\n")
(display "  ")
(display (bench-nbsp))
(display " 秒\n\n")

(display "Unicode 全角空格 (#\\x3000) -> 命中:\n")
(display "  ")
(display (bench-ideographic-space))
(display " 秒\n")
