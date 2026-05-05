;; char-upper-case? vs ascii-upper-case? 性能对比测试
;; char-upper-case? 现基于 s7 hash-table 实现

(import (scheme base)
  (scheme char)
  (liii ascii)
  (liii timeit)
)

(define iterations 100000)

(define (bench-char-upper-case-ascii-upper)
  (timeit (lambda () (char-upper-case? #\A)) '() iterations)
)

(define (bench-ascii-upper-case-ascii-upper)
  (timeit (lambda () (ascii-upper-case? #\A)) '() iterations)
)

(define (bench-char-upper-case-ascii-lower)
  (timeit (lambda () (char-upper-case? #\a)) '() iterations)
)

(define (bench-ascii-upper-case-ascii-lower)
  (timeit (lambda () (ascii-upper-case? #\a)) '() iterations)
)

(display "=== char-upper-case? (hash-table) vs ascii-upper-case? 性能测试 ===\n")
(display "迭代次数: ")
(display iterations)
(display "\n\n")

(display "ASCII 大写 (#\\A):\n")
(display "  char-upper-case?  : ")
(display (bench-char-upper-case-ascii-upper))
(display " 秒\n")
(display "  ascii-upper-case? : ")
(display (bench-ascii-upper-case-ascii-upper))
(display " 秒\n\n")

(display "ASCII 小写 (#\\a):\n")
(display "  char-upper-case?  : ")
(display (bench-char-upper-case-ascii-lower))
(display " 秒\n")
(display "  ascii-upper-case? : ")
(display (bench-ascii-upper-case-ascii-lower))
(display " 秒\n\n")
