(import (liii check) (liii pi) (liii date-time))

(check-set-mode! 'report-failed)

;; pi-current-time-millis
;; 获取当前 epoch 毫秒时间戳，必须返回精确整数（不能是浮点/有理数）。
(let ((ms (pi-current-time-millis)))
  (check-true (number? ms))
  (check-true (exact? ms))
  (check-true (integer? ms))
  ;; 量级与 epoch 毫秒一致
  (check-true (> ms 1700000000000))
  (check-true (< ms 2000000000000))
  ;; 与 (liii date-time) 的 UTC 时间源一致（允许秒级边界滑动）
  (let* ((before (floor (* 1000 (timestamp->utc-posix-time (current-utc-timestamp)))))
         (after (floor (* 1000 (timestamp->utc-posix-time (current-utc-timestamp)))))
        ) ;
    (check-true (>= ms (- before 1000)))
    (check-true (<= ms (+ after 1000)))
  ) ;let*
  ;; 单调不减（墙钟语义）
  (let ((ms2 (pi-current-time-millis)))
    (check-true (exact? ms2))
    (check-true (>= ms2 ms))
  ) ;let
) ;let

(check-report)
