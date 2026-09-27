(import (liii check)
        (liii go))
(check-set-mode! 'report-failed)
;; 有效 channel + type-error（irritant 为整数 -1），无任何关闭/channel irritant
(define ch (make-chan 1))
(check-catch 'type-error (chan-recv! ch -1))
(check-report)
