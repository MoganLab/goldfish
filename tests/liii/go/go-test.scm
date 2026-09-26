(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; 1. 基本 go 任务启动与通道通信
(define ch1 (make-chan 1))
(go (ch1)
  (chan-send! ch1 "hello from worker" 5000))

(define msg1 (chan-recv! ch1 5000))
(check msg1 => "hello from worker")

;; 2. 多个 worker 并发向同一个 channel 写入计算结果
(define results (make-chan 10))
(define (spawn-worker i)
  (go (results i)
    (chan-send! results (* i i) 5000)))

;; 启动 5 个并发任务
(spawn-worker 1)
(spawn-worker 2)
(spawn-worker 3)
(spawn-worker 4)
(spawn-worker 5)

;; 收集所有结果
(define collected '())
(let loop ((count 0))
  (if (< count 5)
      (let ((val (chan-recv! results 5000)))
        (set! collected (cons val collected))
        (loop (+ count 1)))))

;; 验证结果包含 1, 4, 9, 16, 25（顺序可能不同，因为是多核并发）
(check (length collected) => 5)
(check (pair? (member 1 collected)) => #t)
(check (pair? (member 4 collected)) => #t)
(check (pair? (member 9 collected)) => #t)
(check (pair? (member 16 collected)) => #t)
(check (pair? (member 25 collected)) => #t)

(check-report)
