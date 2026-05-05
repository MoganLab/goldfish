;; char-numeric? / char-upper-case? / char-lower-case? 性能对比测试
;; 均已基于 s7 hash-table 实现

(import (scheme base)
  (scheme char)
  (liii ascii)
  (liii timeit)
)

(define iterations 100000)

;; ---------- char-numeric? ----------
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

;; ---------- char-upper-case? ----------
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

;; ---------- char-lower-case? ----------
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

(display "=== char-numeric? / char-upper-case? / char-lower-case? 性能测试 ===\n")
(display "迭代次数: ")
(display iterations)
(display "\n\n")

(display "--- char-numeric? ---\n")
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

(display "--- char-upper-case? ---\n")
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

(display "--- char-lower-case? ---\n")
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
