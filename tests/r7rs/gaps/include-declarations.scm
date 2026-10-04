(import (scheme base) (liii check) (r7rs-audit include-declarations))
(check-set-mode! 'report-failed)
(check answer => 42)
(check-report)
