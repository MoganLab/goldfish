(import (liii check) (liii time))

(check-set-mode! 'report-failed)

;; process-time-ns
;; 返回当前进程累计消耗的 CPU 时间，单位为纳秒。
;;
;; 语法
;; ----
;; (process-time-ns)
;;
;; 返回值
;; ----
;; non-negative-integer?
;; 返回非负精确整数，统计范围与 process-time 相同。
;;
;; 注意
;; ----
;; 1. 适合需要整数时间值的性能分析场景
;; 2. 返回值是累计 CPU 时间，应通过两次调用的差值计算 CPU 耗时
;; 3. 实际精度由操作系统决定，不代表每次调用都增加一纳秒
;; 4. 系统接口失败或平台不支持时抛出 os-error

(let ((nanoseconds (process-time-ns)))
  (check (integer? nanoseconds) => #t)
  (check (exact? nanoseconds) => #t)
  (check (>= nanoseconds 0) => #t))

(let ((start (process-time-ns)))
  (do ((i 0 (+ i 1))
       (n 0 (+ n i)))
      ((= i 10000)
       (check (>= n 0) => #t)
       (check (>= (- (process-time-ns) start) 0) => #t))))

(check-report)
