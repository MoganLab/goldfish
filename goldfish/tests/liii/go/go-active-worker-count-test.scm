;; go-active-worker-count
;; 返回当前已创建的 worker 线程数量。
;;
;; 语法
;; ----
;; (go-active-worker-count)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; integer
;; 当前活跃的 worker 线程数；线程池尚未创建时为 0。
;;
;; 说明
;; ----
;; 线程池按需创建：只有派发后台任务后才会真正创建 worker 会话，
;; 因此该值通常小于等于 go-worker-count 返回的配置上限。

(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; 1. 未派发任务时线程池尚未创建

(check (go-active-worker-count) => 0)

;; 2. 派发一个任务后创建了一个 worker

(define ch (make-chan 1))

(define (worker-task ch)
  (chan-send! ch 'done)
) ;define

(go (worker-task ch))
(check (chan-recv! ch 5000) => 'done)
(check (go-active-worker-count) => 1)

(check-report)
