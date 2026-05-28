(import (liii check) (liii subprocess))

(run-set! 'echo "/bin/echo")
(check (zero? (run '(echo "hello"))) => #t)

(run-set! 'true "/bin/true")
(check (zero? (run '(true))) => #t)

(check-report)
