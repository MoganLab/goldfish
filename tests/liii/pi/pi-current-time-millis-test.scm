(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-current-time-millis
;; 获取当前时间戳毫秒数。
(let ((ms (pi-current-time-millis)))
  (check-true (number? ms))
  (check-true (> ms 1700000000000)))

(check-report)
