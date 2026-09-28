(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; make-timeout-context
;; 创建一个带超时的上下文，到期自动取消。
;;
;; 语法
;; ----
;; (make-timeout-context ms)
;;
;; 参数
;; ----
;; ms : integer?
;; 超时毫秒数。
;;
;; 返回值
;; ----
;; context
;; 新的上下文对象，ms 毫秒后自动完成（done）。
;;
;; 说明
;; ----
;; 超时由 C++ 层独立定时线程驱动，不占用 worker 线程池。

(define ctx (make-timeout-context 50))
(check (context-done? ctx) => #f)
;; 阻塞等待取消信号（通道关闭后 recv 返回 eof）
(check (eof-object? (chan-recv! (context-channel ctx) 2000)) => #t)
(check (context-done? ctx) => #t)

;; 回归：大量长超时 context 不占用 worker 线程池（曾导致池饿死）
(let loop
  ((i 0))
  (when (< i (go-worker-count))
    (make-timeout-context 3000)
    (loop (+ i 1))
  ) ;when
) ;let

(define probe-ch (make-chan 1))
(go (probe-ch) (chan-send! probe-ch "pong"))
(check (chan-recv! probe-ch 1000 'starved) => "pong")

(check-report)
