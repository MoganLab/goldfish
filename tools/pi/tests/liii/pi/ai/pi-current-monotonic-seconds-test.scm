(import (liii check) (liii pi ai time) (liii date-time))

(check-set-mode! 'report-failed)

;; pi-current-monotonic-seconds
;; 获取系统自开机以来的单调整秒数，返回精确整数。
(let ((sec (pi-current-monotonic-seconds)))
  (check-true (number? sec))
  (check-true (exact? sec))
  (check-true (integer? sec))
  (check-true (>= sec 0))
  (check-true (< sec 1000000000))
  (check-true (> (pi-current-time-seconds) 1700000000))
  (let ((sec2 (pi-current-monotonic-seconds)))
    (check-true (exact? sec2))
    (check-true (>= sec2 sec))
  ) ;let
  (let* ((sec-before (pi-current-monotonic-seconds))
         (ms (pi-current-monotonic-millis))
         (sec-after (pi-current-monotonic-seconds))
        ) ;
    (check-true (<= (* sec-before 1000) ms))
    (check-true (< ms (* (+ sec-after 1) 1000)))
  ) ;let*

  ;; 回拨场景与超时判定模型验证：
  ;; 验证单调钟判定在 NTP 回拨或系统时间修改时不会失效
  (let ((idle-timeout? (lambda (now last-act idle)
                         (and idle (> (- now last-act) idle))
                       ) ;lambda
        ) ;idle-timeout?
        (session-timeout? (lambda (now deadline)
                            (and deadline (> now deadline))
                          ) ;lambda
        ) ;session-timeout?
       ) ;
    ;; 场景 A: 空闲超时判定
    ;; 若使用墙钟发生 NTP 回拨 (-50s)：last-act=1000, wall-now=950, idle=10
    ;; (- 950 1000) = -50 <= 10 -> 超时判定失败（静默挂起）
    (check-false (idle-timeout? 950 1000 10))
    ;; 单调钟模式下时钟严格递增 (+15s)：mono-last=100, mono-now=115, idle=10
    ;; (- 115 100) = 15 > 10 -> 超时正确触发
    (check-true (idle-timeout? 115 100 10))

    ;; 场景 B: 会话超时判定
    ;; 若使用墙钟发生 NTP 回拨：start=1000, timeout=10, deadline=1010
    ;; 经过 15s 后墙钟回拨至 950 -> now < deadline，超时无法触发（静默挂起）
    (check-false (session-timeout? 950 1010))
    ;; 单调钟模式下时钟递增：start=100, timeout=10, deadline=110, mono-now=115
    ;; now > deadline -> 超时正确触发
    (check-true (session-timeout? 115 110))
  ) ;let
) ;let

(check-report)
