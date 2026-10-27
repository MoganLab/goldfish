(import (liii check) (liii pi ai time))

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

;; 与单调秒接口的换算关系
(let* ((sec-before (pi-current-monotonic-seconds))
       (ms (pi-current-monotonic-millis))
       (sec-after (pi-current-monotonic-seconds))
      ) ;
  (check-true (<= (* sec-before 1000) ms))
  (check-true (< ms (* (+ sec-after 1) 1000)))
) ;let*

(check-report)
