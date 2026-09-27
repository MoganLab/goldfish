(import (liii check)
        (liii go))
(check-set-mode! 'report-failed)
;; 普通 define 包装 + 整数 irritant（timeout 参数校验路径）
(define ch (make-chan 1))
(check-catch 'type-error (chan-recv! ch -1))
(check-report)
