(import (scheme base) (liii check) (r7rs-audit 7))
(check-set-mode! 'report-failed)
(check answer => 42)
(check-report)
