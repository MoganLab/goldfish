(import (liii check) (liii subprocess))

(run-ban! 'dangerous-cmd)
(check-catch 'value-error (run '(dangerous-cmd "arg")))

(check-report)
