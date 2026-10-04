(import (scheme base) (scheme lazy) (liii check))
(check-set-mode! 'report-failed)

;; 4.2.5: delay caches the expression's value without recursively forcing it.
(let* ((runs 0)
       (inner (delay (begin (set! runs (+ runs 1)) 7)))
       (outer (delay inner)))
  (check (eq? (force outer) inner) => #t)
  (check runs => 0)
  (check (force (force outer)) => 7)
  (check runs => 1)
  (check (eq? (make-promise inner) inner) => #t))

;; The first completed computation wins when forcing a promise reentrantly.
(let ((p #f) (entered? #f))
  (set! p (delay (if entered? 'inner
                    (begin (set! entered? #t) (force p) 'outer))))
  (check (force p) => 'inner)
  (check (force p) => 'inner))

(let ((p #f) (counter 5))
  (set! p (delay (if (zero? counter) 0
                    (begin (set! counter (- counter 1))
                           (force p)
                           (set! counter (+ counter 2))
                           counter))))
  (check (force p) => 0)
  (check counter => 10)
  (check (force p) => 0))

;; A continuation returning to a completed promise must not overwrite its value.
(let ((saved #f) (resumed? #f) (p #f))
  (set! p (delay (call/cc (lambda (k) (set! saved k) 'first))))
  (let ((result (force p)))
    (if resumed?
      (begin (check result => 'first) (check (force p) => 'first))
      (begin (set! resumed? #t) (saved 'later)))))

;; The dynamic parameter environment comes from the first force, not delay.
(let* ((setting (make-parameter 'creation))
       (p (delay (setting))))
  (check (parameterize ((setting 'forcing)) (force p)) => 'forcing)
  (check (parameterize ((setting 'later)) (force p)) => 'forcing))

;; Tail promises merge their memoization, including aliases retained by callers.
(let* ((runs 0)
       (inner (delay (begin (set! runs (+ runs 1)) 42)))
       (middle (delay-force inner))
       (outer (delay-force middle)))
  (check (force outer) => 42)
  (check (force middle) => 42)
  (check (force inner) => 42)
  (check runs => 1))

(define (countdown n)
  (delay-force (if (zero? n) (delay 'done) (countdown (- n 1)))))
(let ((p (countdown 12000)))
  (check (force p) => 'done)
  (check (force p) => 'done))

(check-report)
