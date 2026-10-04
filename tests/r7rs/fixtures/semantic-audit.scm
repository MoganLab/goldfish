;; Each observation retains the standard expectation, including known gaps.
(define (audit-check id thunk expected)
  (display ";;;AUDIT-START ") (display id) (newline)
  (flush-output-port (current-output-port))
  (let* ((actual (guard (exception
                         (else (list 'unexpected-error
                                 (if (error-object? exception)
                                     (error-object-message exception) exception))))
                   (thunk)))
         (passed? (equal? actual expected)))
    (check (list id actual) => (list id expected))
    (display ";;;AUDIT ") (display id)
    (display (if passed? " pass" " gap"))
    (newline)
    (display ";;;RESULT ") (display id) (display " ") (write actual)
    (newline) (flush-output-port (current-output-port))))
(define (audit-eval datum)
  (eval datum (environment '(scheme base))))
