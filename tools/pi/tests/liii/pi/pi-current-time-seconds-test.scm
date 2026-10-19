(import (liii check) (liii pi) (liii date-time))

(check-set-mode! 'report-failed)

;; pi-current-time-seconds
;; 获取当前 epoch 整秒时间戳，必须返回精确整数（截断小数秒）。
(let ((secs (pi-current-time-seconds)))
  (check-true (number? secs))
  (check-true (exact? secs))
  (check-true (integer? secs))
  ;; 量级与 epoch 秒一致
  (check-true (> secs 1700000000))
  (check-true (< secs 2000000000))
  ;; 与 (liii date-time) 的 UTC 时间源一致（允许秒级边界滑动）
  (let* ((before (floor (timestamp->utc-posix-time (current-utc-timestamp))))
         (after (floor (timestamp->utc-posix-time (current-utc-timestamp))))
        ) ;
    (check-true (>= secs (- before 1)))
    (check-true (<= secs (+ after 1)))
  ) ;let*
  ;; 与毫秒接口的截断关系：采样时刻夹在两次整秒读数之间时
  ;; secs-before*1000 <= millis < (secs-after+1)*1000
  (let* ((secs-before (pi-current-time-seconds))
         (ms (pi-current-time-millis))
         (secs-after (pi-current-time-seconds))
        ) ;
    (check-true (<= (* secs-before 1000) ms))
    (check-true (< ms (* (+ secs-after 1) 1000)))
    ;; 单调不减（墙钟语义）
    (check-true (>= secs-after secs-before))
    (check-true (>= secs-before secs))
  ) ;let*
) ;let

(check-report)
