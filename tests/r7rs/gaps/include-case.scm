(import (scheme base) (liii check) (r7rs-audit include-case))
(check-set-mode! 'report-failed)
(check included-value => 42)
(check-report)
