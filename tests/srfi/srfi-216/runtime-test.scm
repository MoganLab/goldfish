;; runtime
;; 返回基于 `current-second` 换算的毫秒时间戳。
;;
;; 语法
;; ----
;; (runtime)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受任何参数。
;;
;; 返回值
;; ----
;; real?
;; 返回以毫秒计的时间戳（由 `current-second` 乘 1000 后取整得到）。
;;
;; 说明
;; ----
;; 由于 `current-second` 返回非精确实数，`runtime` 的结果同样是非精确实数，
;; 取整后其数值部分为整数毫秒数；两次调用之间的差值可用于粗略测量耗时。

(import (srfi sicp) (liii check) (liii os))

(check-set-mode! 'report-failed)

(let ((t1 (runtime)))
  (check (real? t1) => #t)
  (check (>= t1 0) => #t)
  (when (os-linux?)
    (os-call "sleep 0.01")
  ) ;when
  (let ((t2 (runtime)))
    (check (>= t2 t1) => #t)
  ) ;let
) ;let

(check-report)
