(import (liii check) (liii subprocess))

(run-set! 'echo-cmd "/bin/echo")
(check (zero? (run '(echo-cmd "hello"))) => #t)

(run-set! 'true-cmd "/bin/true")
(check (zero? (run '(true-cmd))) => #t)

(run-set! 'my-lambda (lambda () (display "ok\n")))
(check (zero? (run '(my-lambda))) => #t)

(check-report)
