(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-current-monotonic-millis
;; 获取系统自开机以来的单调毫秒数，返回精确整数。
(let ((ms (pi-current-monotonic-millis)))
  (check-true (number? ms))
  (check-true (exact? ms))
  (check-true (integer? ms))
  (check-true (> ms 0))
  (check-true (< ms 1000000000000))
  (let ((ms2 (pi-current-monotonic-millis)))
    (check-true (exact? ms2))
    (check-true (>= ms2 ms))
  ) ;let
) ;let

(check-report)
