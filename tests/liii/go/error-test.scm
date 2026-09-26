(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; 1. 测试 Worker 内部发生运行时错误时不崩溃，且能继续执行后续任务
(define ch-err (make-chan 1))
(define ch-healthy (make-chan 1))

;; 派发一个会报错的任务（除以零）
(go (ch-err)
  (/ 1 0)
  (chan-send! ch-err "never-reached"))

;; 验证失败任务关联的通道会被自动关闭以解开等待者
(check (eof-object? (chan-recv! ch-err 2000)) => #t)

;; 确保 Worker 线程池没有崩溃，继续派发一个正常任务
(go (ch-healthy)
  (chan-send! ch-healthy "healthy-result" 1000))

(check (chan-recv! ch-healthy 2000) => "healthy-result")

;; 2. 测试任务内部自定义 error 抛出时 Worker 正常容错
(define ch-ok (make-chan 1))
(go ()
  (error 'test-error "simulated task crash"))

(go (ch-ok)
  (chan-send! ch-ok 12345 1000))

(check (chan-recv! ch-ok 2000) => 12345)

(check-report)
