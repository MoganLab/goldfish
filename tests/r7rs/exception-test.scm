(import (scheme base) (scheme lazy) (liii check))
(check-set-mode! 'report-failed)

(check (with-exception-handler (lambda (obj) obj)
         (lambda () (+ 1 (raise-continuable 3)))) => 4)
(check (call-with-values
         (lambda () (with-exception-handler (lambda (obj) (values 1 2))
                      (lambda () (raise-continuable 'many)))) list) => '(1 2))
(check (call-with-values
         (lambda () (with-exception-handler (lambda (obj) (values))
                      (lambda () (raise-continuable 'none)))) list) => '())

;; A handler is inactive while it handles an exception, then restored.
(let ((outer-calls 0) (inner-calls 0))
  (check (with-exception-handler
           (lambda (obj) (set! outer-calls (+ outer-calls 1)) (* obj 10))
           (lambda ()
             (with-exception-handler
               (lambda (obj)
                 (set! inner-calls (+ inner-calls 1))
                 (+ 1 (raise-continuable obj)))
               (lambda () (+ (raise-continuable 1) (raise-continuable 2)))))) => 32)
  (check (list outer-calls inner-calls) => '(2 2)))

;; Returning from a non-continuable raise signals a new exception.
(check (guard (condition (else (error-object? condition)))
         (with-exception-handler (lambda (obj) obj)
           (lambda () (raise 'oops)))) => #t)
(check (call/cc (lambda (escape)
         (with-exception-handler (lambda (obj) (escape (list 'handled obj)))
           (lambda () (raise 'oops))))) => '(handled oops))

;; Calling a primitive handler can itself fail and reach the outer handler.
(check (call/cc (lambda (escape)
         (with-exception-handler (lambda (obj) (escape (error-object? obj)))
           (lambda ()
             (with-exception-handler car
               (lambda () (raise-continuable 3))))))) => #t)

;; Handler execution uses the raising dynamic environment.
(let ((p (make-parameter 1)) (events '()))
  (define (record value) (set! events (cons value events)))
  (check (with-exception-handler
           (lambda (obj) (record (list 'handler (p))) obj)
           (lambda ()
             (parameterize ((p 2))
               (dynamic-wind (lambda () (record 'before))
                 (lambda () (+ 1 (raise-continuable 3)))
                 (lambda () (record 'after)))))) => 4)
  (check (reverse events) => '(before (handler 2) after))
  (check (p) => 1))

;; Capturing inside a handler preserves its mask across multiple invocations.
(let ((saved #f) (phase 0) (observed '()))
  (let ((answer
          (with-exception-handler
            (lambda (obj) (+ obj (call/cc (lambda (k) (set! saved k) 1))))
            (lambda () (raise-continuable 3)))))
    (set! observed (cons answer observed))
    (cond ((= phase 0) (set! phase 1) (saved 10))
          ((= phase 1) (set! phase 2) (saved 20))
          (else (check (reverse observed) => '(4 13 23))))))

;; An unmatched guard forwards at the raising site, then resumes there.
(let ((p (make-parameter 1)) (events '()))
  (define (record value) (set! events (cons value events)))
  (check (with-exception-handler
           (lambda (obj) (record (list 'handler (p))) obj)
           (lambda ()
             (guard (condition ((begin (check (p) => 1) #f) 'unreachable))
               (parameterize ((p 2))
                 (dynamic-wind (lambda () (record 'before))
                   (lambda () (+ 1 (raise-continuable 3)))
                   (lambda () (record 'after))))))) => 4)
  (check (reverse events) => '(before after before (handler 2) after))
  (check (p) => 1))

;; A delayed computation uses the handler active when it is first forced.
(let ((calls 0) (promise (delay (+ 1 (raise-continuable 'delayed)))))
  (check (with-exception-handler
           (lambda (obj) (set! calls (+ calls 1)) 41)
           (lambda () (force promise))) => 42)
  (check (force promise) => 42)
  (check calls => 1))

(check-report)
