(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; 1. 测试基础 Context 手动取消
(define ctx1 (make-context))
(check (context-done? ctx1) => #f)

(define ch-done (context-channel ctx1))
(check (chan? ch-done) => #t)
(check (chan-closed? ch-done) => #f)

;; 执行取消
(context-cancel! ctx1)
(check (context-done? ctx1) => #t)
(check (chan-closed? ch-done) => #t)
(check (eof-object? (chan-recv! ch-done 100)) => #t)

;; 重复 cancel 安全无害
(context-cancel! ctx1)
(check (context-done? ctx1) => #t)

;; 2. 测试带有超时的 Timeout Context (自动取消)
(define ctx-to (make-timeout-context 50)) ; 50 毫秒后自动超时取消
(check (context-done? ctx-to) => #f)

;; 等待 100ms 后检查
(chan-recv! (context-channel ctx-to) 1000)
(check (context-done? ctx-to) => #t)

;; 3. 测试与 select 和 go 协程结合的协作式任务取消
(define ctx-worker (make-context))
(define ch-status (make-chan 1))

(go (ctx-worker ch-status)
  (let loop ()
    (select
      ((chan-recv! (context-channel ctx-worker) _)
       ;; 收到取消信号，优雅退出并汇报
       (chan-send! ch-status "stopped-by-context"))
      (timeout 10
       ;; 继续工作
       (loop)))))

;; 主线程发号施令取消该任务
(context-cancel! ctx-worker)
(check (chan-recv! ch-status 2000) => "stopped-by-context")

(check-report)
