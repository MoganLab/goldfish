;; pi-current-monotonic-seconds
;; 获取系统自开机以来的单调整秒数（精确整数）。
;;
;; 语法
;; ----
;; (pi-current-monotonic-seconds)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; integer?
;; 单调递增的整秒时间戳整数。

(import (liii check) (liii pi ai time) (liii date-time))

(check-set-mode! 'report-failed)

(let ((sec (pi-current-monotonic-seconds)))
  (check-true (number? sec))
  (check-true (exact? sec))
  (check-true (integer? sec))
  (check-true (>= sec 0))
  (check-true (< sec 1000000000))
  (let ((sec2 (pi-current-monotonic-seconds)))
    (check-true (exact? sec2))
    (check-true (>= sec2 sec))
  ) ;let
  ;; 回拨场景与超时判定模型验证
  (let ((idle-timeout?
          (lambda (now last-act idle) (and idle (> (- now last-act) idle)))
        ) ;idle-timeout?
        (session-timeout? (lambda (now deadline) (and deadline (> now deadline))))
       ) ;
    (check-false (idle-timeout? 950 1000 10))
    (check-true (idle-timeout? 115 100 10))
    (check-false (session-timeout? 950 1010))
    (check-true (session-timeout? 115 110))
  ) ;let
) ;let

(check-report)
