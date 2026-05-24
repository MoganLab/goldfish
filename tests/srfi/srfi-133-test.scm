(import (srfi srfi-133) (srfi srfi-78))

(check-set-mode! 'report-failed)

(define (cmp a b)
  (cond
    ((< a b) -1)
    ((= a b) 0)
    (else 1)))

(define v '#(0 2 4 6 8 10 12))

;; vector-binary-search
(check (vector-binary-search v 0 cmp) => 0)
(check (vector-binary-search v 6 cmp) => 3)
(check (vector-binary-search v 12 cmp) => 6)
(check (vector-binary-search v 1 cmp) => #f)
(check (vector-binary-search v 13 cmp) => #f)
(check (vector-binary-search v -1 cmp) => #f)

;; with start
(check (vector-binary-search v 6 cmp 2) => 3)
(check (vector-binary-search v 0 cmp 2) => #f)

;; with start and end
(check (vector-binary-search v 6 cmp 2 5) => 3)
(check (vector-binary-search v 6 cmp 1 4) => 3)
(check (vector-binary-search v 8 cmp 1 4) => #f)

(check-report)
