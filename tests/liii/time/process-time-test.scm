(import (liii check) (liii time))

(check-set-mode! 'report-failed)

;; process-time
;; 返回当前进程累计消耗的 CPU 时间，单位为秒。
;;
;; 语法
;; ----
;; (process-time)
;;
;; 返回值
;; ----
;; non-negative-real?
;; 返回非负非精确实数，包含进程内所有线程的用户态和内核态 CPU 时间。
;;
;; 注意
;; ----
;; 1. 不包含睡眠、等待 I/O、等待锁的时间，也不包含子进程的 CPU 时间
;; 2. 起点不对应 Unix epoch，应通过两次调用的差值计算 CPU 耗时
;; 3. 不应与 current-second 或 thread-time 的绝对值直接比较
;; 4. 系统接口失败或平台不支持时抛出 os-error
;;
;; 示例
;; ----
;; (define start (process-time))
;; (heavy-computation)
;; (define cpu-seconds (- (process-time) start))

(let ((seconds (process-time)))
  (check (real? seconds) => #t)
  (check (inexact? seconds) => #t)
  (check (>= seconds 0.0) => #t))

(let ((start (process-time)))
  (do ((i 0 (+ i 1))
       (n 0 (+ n i)))
      ((= i 10000)
       (check (>= n 0) => #t)
       (check (>= (- (process-time) start) 0.0) => #t))))

(check-report)
