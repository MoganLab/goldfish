(import (scheme base) (scheme write) (liii check))
(check-set-mode! 'report-failed)

;; Convert once on entry and restore the already converted outer value.
(let ((conversions 0))
  (let ((p (make-parameter 1 (lambda (v) (set! conversions (+ conversions 1)) (+ v 1)))))
    (check (p) => 2)
    (check (parameterize ((p 10)) (p)) => 11)
    (check (p) => 2)
    (check conversions => 2)))

;; Evaluate all binding expressions before changing any parameter.
(let ((p (make-parameter 1)) (q (make-parameter 2)))
  (check (parameterize ((p 10) (q (p))) (list (p) (q))) => '(10 1))
  (check (list (p) (q)) => '(1 2)))
(let ((p (make-parameter 1)) (parameter-evaluations 0) (value-evaluations 0))
  (check (parameterize (((begin (set! parameter-evaluations (+ parameter-evaluations 1)) p)
                         (begin (set! value-evaluations (+ value-evaluations 1)) 10)))
           (p)) => 10)
  (check (list parameter-evaluations value-evaluations) => '(1 1)))

;; Re-entry preserves the inner mutation without repeating conversions.
(let ((saved #f) (phase 0) (inner-visited? #f) (conversions 0))
  (let ((p (make-parameter 1 (lambda (v) (set! conversions (+ conversions 1)) (+ v 1)))))
    (parameterize ((p 10))
      (call/cc (lambda (k) (set! saved k)))
      (if inner-visited?
        (check (p) => 21)
        (begin (p 20) (set! inner-visited? #t))))
    (if (zero? phase)
      (begin (set! phase 1) (saved 'again))
      (begin (check (p) => 2) (check conversions => 3)))))

;; Conversion failure leaves every outer binding intact.
(let ((p (make-parameter 1 (lambda (v) (+ v 1))))
      (q (make-parameter 0 (lambda (v) (if (negative? v) (error "negative") v)))))
  (check (guard (condition (else 'caught))
           (parameterize ((p 10) (q -1)) 'unreachable)) => 'caught)
  (check (list (p) (q)) => '(2 0)))

;; Port parameters use the same dynamic binding protocol.
(let ((original (current-output-port)) (port (open-output-string)))
  (parameterize ((current-output-port port)) (display "hello"))
  (check (get-output-string port) => "hello")
  (check (eq? (current-output-port) original) => #t))
(let ((original (current-input-port)) (port (open-input-string "x")))
  (check (parameterize ((current-input-port port)) (read-char)) => #\x)
  (check (eq? (current-input-port) original) => #t))
(let ((original (current-error-port)) (port (open-output-string)))
  (check (parameterize ((current-error-port port)) (eq? (current-error-port) port)) => #t)
  (check (eq? (current-error-port) original) => #t))

(check-report)
