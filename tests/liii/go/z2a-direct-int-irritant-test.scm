(import (liii check)
        (liii go))
(check-set-mode! 'report-failed)
;; 直接调 C 函数，整数 irritant，无 case-lambda
(check-catch 'type-error (g_make-chan -1))
(check-report)
