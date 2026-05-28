(import (liii check) (liii subprocess))

(run-set! 'echo-cmd "/bin/echo")
(run-set! 'true-cmd "/bin/true")

(run-allow! 'echo-cmd)
(check (zero? (run '(echo-cmd "hello"))) => #t)
(check-catch 'value-error (run '(true-cmd)))

(run-allow! '())
(check (zero? (run '(true-cmd))) => #t)

(run-allow! '(echo-cmd true-cmd))
(check (zero? (run '(echo-cmd "hello"))) => #t)
(check (zero? (run '(true-cmd))) => #t)
(check-catch 'value-error (run '(unknown-cmd)))

(run-allow! '())

(check-report)
