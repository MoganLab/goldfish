(import (liii check)
        (liii go))
(check-set-mode! 'report-failed)
;; 经 case-lambda 包装，整数 irritant
(check-catch 'type-error (make-chan -1))
(check-report)
