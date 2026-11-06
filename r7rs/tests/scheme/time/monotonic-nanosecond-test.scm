;; monotonic-nanosecond
;; 获取单调时钟的纳秒计数。
;;
;; 语法
;; ----
;; (monotonic-nanosecond)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ----
;; integer
;; 自某个固定起点以来的纳秒数，随时间单调递增，不受系统时间调整影响。
;;
;; 说明
;; ----
;; 起点未定义，只应比较两次读数的差值，用于测量时间间隔。

(import (liii check) (scheme time))

(check-set-mode! 'report-failed)

(let ((t1 (monotonic-nanosecond)))
  (check (integer? t1) => #t)
  (check (> t1 0) => #t)
  (let ((t2 (monotonic-nanosecond)))
    ;; 单调时钟不会回退
    (check (>= t2 t1) => #t)
  ) ;let
) ;let

(check-report)
