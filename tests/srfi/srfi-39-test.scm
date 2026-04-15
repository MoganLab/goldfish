
(import (srfi srfi-78) (srfi srfi-39))

(check-set-mode! 'report-failed)

(define mp
  (make-parameter "initial value")
) ;define

(check (mp) => "initial value")

(check (parameterize ((mp "new value"))
         (mp)
       ) ;parameterize
  =>
  "new value"
) ;check

(check (mp) => "initial value")

(check-report)
(if (check-failed?) (exit -1))
