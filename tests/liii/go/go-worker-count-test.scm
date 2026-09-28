(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; go-worker-count
;; 返回后台 worker 线程池的大小。
;;
;; 语法
;; ----
;; (go-worker-count)
;;
;; 返回值
;; ----
;; integer
;; worker 线程数，等于系统硬件核心数（无法探测时为 4）。
;;
;; 说明
;; ----
;; 该调用不会触发线程池的创建，只返回配置值。

(define n (go-worker-count))
(check (integer? n) => #t)
(check (> n 0) => #t)

(check-report)
