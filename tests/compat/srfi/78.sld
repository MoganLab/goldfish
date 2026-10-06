;;
;; SRFI 78 compatibility shim for Chibi Scheme and R7RS implementations
;;

(define-library (srfi 78)
  (export check check-report check-set-mode! check-reset! check-passed?)
  (import (scheme base) (scheme write))
  (begin
    (define check:correct 0)
    (define check:failed 0)

    (define (check-reset!)
      (set! check:correct 0)
      (set! check:failed 0))

    (define (check-set-mode! m) #t)

    (define (check-report)
      (newline)
      (display "; *** checks *** : ")
      (display check:correct)
      (display " correct, ")
      (display check:failed)
      (display " failed.\n"))

    (define (check-passed? n)
      (and (= check:failed 0) (= check:correct n)))

    (define (check:proc expr thunk expected)
      (let ((res (thunk)))
        (if (equal? res expected)
            (set! check:correct (+ check:correct 1))
            (begin
              (set! check:failed (+ check:failed 1))
              (newline)
              (display "*** failed ***\n")
              (display "; expr: ") (write expr) (newline)
              (display "; expected: ") (write expected) (newline)
              (display "; actual:   ") (write res) (newline)))))

    (define-syntax check
      (syntax-rules (=>)
        ((check expr => expected)
         (check:proc (quote expr) (lambda () expr) expected))))))
