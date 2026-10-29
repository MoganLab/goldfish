(import (liii check) (liii go) (liii time))

(check-set-mode! 'report-failed)

;; go-worker-count
;; 返回后台 worker 线程池的配置上限。
;;
;; 语法
;; ----
;; (go-worker-count)
;;
;; 返回值
;; ----
;; integer
;; worker 最大线程数，等于系统硬件核心数（无法探测时为 4）。
;;
;; 说明
;; ----
;; 该调用不会触发线程池的创建，只返回配置值。

(define n (go-worker-count))
(check (integer? n) => #t)
(check (> n 0) => #t)
(check (go-active-worker-count) => 0)

;; 线程池按需创建：第一个后台任务完成后只保留一个实际 worker，
;; 不按 CPU 核数预先创建整套空闲的 S7 会话。

(define lazy-ch (make-chan 1))

(define (lazy-worker-task ch)
  (chan-send! ch 'done)
) ;define
(go (lazy-worker-task lazy-ch))
(check (chan-recv! lazy-ch 3000 'timeout) => 'done)
(check (go-active-worker-count) => 1)

;; 已有 worker 忙于阻塞式 C 调用时，新任务按需触发扩容，保留并行能力。
;;
;; worker 池按“排队任务数超出空闲容量”扩容，扩容时机依赖线程唤醒时序：
;; 单核机器（或线程调度延迟较大的 Windows CI）上，任务入队时上一个 worker
;; 尚未回到等待态，池会多建一个 worker（实际并发执行的任务仍只有 2 个，
;; 额外的 worker 只是等待态）。因此这里不断言 worker 的精确数量，
;; 只断言：已扩容到可并行执行两个任务的规模，且不超过配置上限。

(define (slow-worker-task ch)
  (sleep 0.5)
  (chan-send! ch 'done)
) ;define
(when (> n 1)
  (let ((slow-ch-1 (make-chan 1)) (slow-ch-2 (make-chan 1)))
    (go (slow-worker-task slow-ch-1))
    (go (slow-worker-task slow-ch-2))
    (check (>= (go-active-worker-count) 2) => #t)
    (check (<= (go-active-worker-count) n) => #t)
    (check (chan-recv! slow-ch-1 3000 'timeout) => 'done)
    (check (chan-recv! slow-ch-2 3000 'timeout) => 'done)
  ) ;let
) ;when

(check-report)
