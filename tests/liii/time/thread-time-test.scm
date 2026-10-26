(import (liii check) (liii time))

(check-set-mode! 'report-failed)

;; thread-time
;; 返回当前操作系统线程累计消耗的 CPU 时间，单位为秒。
;;
;; 语法
;; ----
;; (thread-time)
;;
;; 返回值
;; ----
;; non-negative-real?
;; 返回非负非精确实数，包含当前线程的用户态和内核态 CPU 时间。
;;
;; 注意
;; ----
;; 1. 不包含其他线程、睡眠、等待 I/O 或等待锁的 CPU 时间
;; 2. 起点不对应 Unix epoch，应通过两次调用的差值计算 CPU 耗时
;; 3. 线程指操作系统线程，不是 Scheme 协程；同一系统线程上的协程不能分别统计
;; 4. 系统接口失败或平台不支持时抛出 os-error
;;
;; 示例
;; ----
;; (define start (thread-time))
;; (thread-local-computation)
;; (define cpu-seconds (- (thread-time) start))

(let ((seconds (thread-time)))
  (check (real? seconds) => #t)
  (check (inexact? seconds) => #t)
  (check (>= seconds 0.0) => #t))

(let ((start (thread-time)))
  (do ((i 0 (+ i 1))
       (n 0 (+ n i)))
      ((= i 10000)
       (check (>= n 0) => #t)
       (check (>= (- (thread-time) start) 0.0) => #t))))

(check-report)
