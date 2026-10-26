(import (liii check)
        (liii psutil)
        (scheme time))

(check-set-mode! 'report-failed)

;; boot-time
;; 获取系统启动时刻，返回 Unix Epoch 以来的秒数。
;;
;; 语法
;; ----
;; (boot-time) => inexact-real?
;;
;; 返回值
;; ------
;; 非精确实数，单位为秒，与 Python psutil.boot_time() 的语义一致。
;;
;; 错误
;; ----
;; 平台不支持或无法获取启动时间时抛 os-error。

(let-values (((now usec) (get-time-of-day)))
  (let ((time (boot-time)))
    (check-true (real? time))
    (check-true (inexact? time))
    (check-true (> time 0))
    (check-true (<= time now))))

(check-report)
