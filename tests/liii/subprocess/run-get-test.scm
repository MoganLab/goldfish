(import (liii check) (liii subprocess) (liii path))

(run-set! 'ls "/bin/ls")
(check (path? (run-get 'ls)) => #t)
(check (run-get 'not-exist) => #f)

(run-set! 'custom-lambda (lambda () (display "ok\n")))
(check (procedure? (run-get 'custom-lambda)) => #t)

(check-report)
