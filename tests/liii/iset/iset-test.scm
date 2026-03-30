(import (liii check)
        (liii iset)
        (only (srfi srfi-1) every any count)
) ;import

(check-set-mode! 'report-failed)

;;;; Utility

(define (init xs)
  (if (null? (cdr xs))
      '()
      (cons (car xs) (init (cdr xs)))
  ) ;if
) ;define

(define (constantly x)
  (lambda (_) x)
) ;define

(define pos-seq (iota 20 100 3))
(define neg-seq (iota 20 -100 3))
(define mixed-seq (iota 20 -10 3))
(define sparse-seq (iota 20 -10000 1003))

(define pos-set (list->iset pos-seq))
(define pos-set+ (iset-adjoin pos-set 9))
(define neg-set (list->iset neg-seq))
(define mixed-set (list->iset mixed-seq))
(define dense-set (make-range-iset 0 49))
(define sparse-set (list->iset sparse-seq))

(define all-test-sets
  (list pos-set neg-set mixed-set dense-set sparse-set)
) ;define

(check-report)
