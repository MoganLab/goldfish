(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; current-time
;; 获取当前时间。
;;
;; 语法
;; ----
;; (current-time [clock-type])
;;
;; 参数
;; ----
;; clock-type : symbol? (可选) 时钟类型，默认为 TIME-UTC。
;;
;; 返回值
;; ----
;; time? 当前时间的时间对象。
;;
;; 错误处理
;; ----
;; wrong-type-arg 当clock-type不是有效的时间类型常量时抛出错误。

;; Test current-time
(check-true (time? (current-time)))
(check-true (time? (current-time TIME-UTC)))
(check-true (time? (current-time TIME-MONOTONIC)))
(check-true (time? (current-time TIME-TAI)))
(check-catch 'wrong-type-arg (time? (current-time TIME-THREAD)))
(check-catch 'wrong-type-arg (time? (current-time TIME-PROCESS)))
(check-catch 'wrong-type-arg (time? (current-time TIME-DURATION)))

;; Check that returned times have correct types
(check (time-type (current-time TIME-UTC))       => TIME-UTC)
(check (time-type (current-time TIME-MONOTONIC)) => TIME-MONOTONIC)
(check (time-type (current-time TIME-TAI))       => TIME-TAI)
(check-catch 'wrong-type-arg (time-type (current-time TIME-THREAD)))
(check-catch 'wrong-type-arg (time-type (current-time TIME-PROCESS)))
(check-catch 'wrong-type-arg (time-type (current-time TIME-DURATION)))

;; Check that nanoseconds are in valid range
(let ((t (current-time)))
  (check-true (>= (time-nanosecond t) 0))
  (check-true (<= (time-nanosecond t) 999999999))
) ;let

;; Test monotonic time increases
(let ((t1 (current-time TIME-MONOTONIC))
      (t2 (current-time TIME-MONOTONIC)))
  (check-true (or (> (time-second t2) (time-second t1))
                  (and (= (time-second t2) (time-second t1))
                       (>= (time-nanosecond t2) (time-nanosecond t1)))
                  ) ;and
  ) ;check-true
) ;let

;; Test error conditions
(check-catch 'wrong-type-arg (current-time 'invalid-type))

(check-report)
