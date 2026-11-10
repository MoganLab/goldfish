;; pi-current-time-seconds
;; 获取当前 epoch 整秒时间戳（精确整数）。
;;
;; 语法
;; ----
;; (pi-current-time-seconds)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; integer?
;; 秒级 Unix 时间戳整数。

(import (liii check) (liii pi ai time) (liii date-time))

(check-set-mode! 'report-failed)

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
  ;; 与毫秒接口的截断关系
  (let* ((secs-before (pi-current-time-seconds))
         (ms (pi-current-time-millis))
         (secs-after (pi-current-time-seconds))
        ) ;
    (check-true (<= (* secs-before 1000) ms))
    (check-true (< ms (* (+ secs-after 1) 1000)))
    (check-true (>= secs-after secs-before))
    (check-true (>= secs-before secs))
  ) ;let*
) ;let

(check-report)
