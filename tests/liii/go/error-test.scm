(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; 1. 测试 Worker 内部发生运行时错误时不崩溃，且能继续执行后续任务

(define (div-zero-task ch)
  (/ 1 0)
  (chan-send! ch "never-reached")
) ;define

(define (healthy-task ch)
  (chan-send! ch "healthy-result" 1000)
) ;define

(define ch-err (make-chan 1))

(define ch-healthy (make-chan 1))

;; 派发一个会报错的任务（除以零）
(go (div-zero-task ch-err))

;; 验证失败任务不会向通道发送垃圾数据，且不会误关通道影响其他任务（超时安全返回）
(check (chan-recv! ch-err 200 'task-failed) => 'task-failed)

;; 确保 Worker 线程池没有崩溃，继续派发一个正常任务
(go (healthy-task ch-healthy))

(check (chan-recv! ch-healthy 2000) => "healthy-result")

;; 2. 测试任务内部自定义 error 抛出时 Worker 正常容错

(define (crash-task)
  (error 'test-error "simulated task crash")
) ;define

(define (send-ok-task ch)
  (chan-send! ch 12345 1000)
) ;define

(define ch-ok (make-chan 1))
(go (crash-task))

(go (send-ok-task ch-ok))

(check (chan-recv! ch-ok 2000) => 12345)

(check-report)
