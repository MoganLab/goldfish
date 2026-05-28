(import (liii check) (liii subprocess))

(run-set! 'echo-cmd "/bin/echo")
(run-ban! 'echo-cmd)
(check-catch 'value-error (run '(echo-cmd "hello")))

(check-report)
