(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; 1. 测试 default 非阻塞分支：通道为空时立即走 default
(define ch1 (make-chan 1))
(define ch2 (make-chan 1))

(define hit-default #f)
(select
  ((chan-recv! ch1 v)
   (set! hit-default 'ch1))
  ((chan-recv! ch2 v)
   (set! hit-default 'ch2))
  (default
   (set! hit-default #t)))

(check hit-default => #t)

;; 2. 测试 chan-recv! 就绪分支
(chan-send! ch1 "hello-select")

(define recv-val #f)
(select
  ((chan-recv! ch1 v)
   (set! recv-val v))
  ((chan-recv! ch2 v)
   (set! recv-val "wrong"))
  (default
   (set! recv-val "default")))

(check recv-val => "hello-select")

;; 3. 测试 chan-send! 就绪分支
(define ch-send (make-chan 1))
(define sent-ok #f)

(select
  ((chan-send! ch-send 999)
   (set! sent-ok #t))
  (default
   (set! sent-ok #f)))

(check sent-ok => #t)
(check (chan-recv! ch-send) => 999)

;; 4. 测试 timeout 超时分支
(define timeout-hit #f)
(define ch-empty (make-chan 1))

(select
  ((chan-recv! ch-empty v)
   (set! timeout-hit 'recv))
  (timeout 50
   (set! timeout-hit 'timeout)))

(check timeout-hit => 'timeout)

;; 5. 测试多核并发下的 select：后台协程延迟发送，主协程 select 阻塞命中
(define ch-async (make-chan 1))
(go (ch-async)
  ;; 稍微工作一下后发送数据
  (letrec ((fib (lambda (x) (if (<= x 1) x (+ (fib (- x 1)) (fib (- x 2)))))))
    (fib 30))
  (chan-send! ch-async "async-ready"))

(define async-result #f)
(select
  ((chan-recv! ch-async msg)
   (set! async-result msg))
  (timeout 2000
   (set! async-result 'timed-out)))

(check async-result => "async-ready")

(check-report)
