;; char-lower-case? vs ascii-lower-case? 性能对比测试
;; char-lower-case? 现基于 s7 hash-table 实现

(import (scheme base)
  (scheme char)
  (liii ascii)
  (liii timeit)
)

(define iterations 100000)

(define (bench-char-lower-case-ascii-lower)
  (timeit (lambda () (char-lower-case? #\a)) '() iterations)
)

(define (bench-ascii-lower-case-ascii-lower)
  (timeit (lambda () (ascii-lower-case? #\a)) '() iterations)
)

(define (bench-char-lower-case-ascii-upper)
  (timeit (lambda () (char-lower-case? #\A)) '() iterations)
)

(define (bench-ascii-lower-case-ascii-upper)
  (timeit (lambda () (ascii-lower-case? #\A)) '() iterations)
)

(display "=== char-lower-case? (hash-table) vs ascii-lower-case? 性能测试 ===\n")
(display "迭代次数: ")
(display iterations)
(display "\n\n")

(display "ASCII 小写 (#\\a):\n")
(display "  char-lower-case?  : ")
(display (bench-char-lower-case-ascii-lower))
(display " 秒\n")
(display "  ascii-lower-case? : ")
(display (bench-ascii-lower-case-ascii-lower))
(display " 秒\n\n")

(display "ASCII 大写 (#\\A):\n")
(display "  char-lower-case?  : ")
(display (bench-char-lower-case-ascii-upper))
(display " 秒\n")
(display "  ascii-lower-case? : ")
(display (bench-ascii-lower-case-ascii-upper))
(display " 秒\n\n")
