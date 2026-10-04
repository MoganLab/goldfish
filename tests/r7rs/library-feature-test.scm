(import (scheme base) (liii check))
(check-set-mode! 'report-failed)
(check (cond-expand ((library (scheme base)) #t) (else #f)) => #t)
(check-report)
