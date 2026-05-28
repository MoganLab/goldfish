(import (liii check) (liii subprocess))

(run-allow! 'echo)
(check (zero? (run '(echo "hello"))) => #t)
(check-catch 'value-error (run '(ls "/tmp")))

(run-allow! '())
(check (zero? (run '(ls "/tmp"))) => #t)

(run-allow! '(echo true))
(check (zero? (run '(echo "hello"))) => #t)
(check (zero? (run '(true))) => #t)
(check-catch 'value-error (run '(ls "/tmp")))

(run-allow! '())

(check-report)
