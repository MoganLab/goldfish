(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; select
;; 多路复用：等待多个通道操作中第一个就绪者（Go select 语义）。
;;
;; 语法
;; ----
;; (select
;;   ((chan-recv! ch var) body ...)
;;   ((chan-send! ch expr) body ...)
;;   (timeout ms body ...)
;;   (default body ...))
;;
;; 子句
;; ----
;; ((chan-recv! ch var) body ...) : 通道可读时，将值绑定到 var 并执行 body
;; ((chan-send! ch expr) body ...) : 通道可写时，发送 expr 并执行 body
;; (timeout ms body ...) : 超过 ms 毫秒仍无就绪分支时执行 body
;; (default body ...) : 无阻塞立即执行（与 timeout 互斥）
;;
;; 说明
;; ----
;; 1. 各分支的通道与发送表达式只在进入 select 时求值一次（Go 语义）。
;; 2. 多个分支同时就绪时按书写顺序选择（无 Go 的随机性）。
;; 3. 当前为 1ms 粒度轮询实现，C++ wait-set 化在演进路线中。
;;
;; 错误处理
;; ----
;; default 与 timeout 同时出现、或同一类子句重复出现时抛出 syntax-error。

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

;; 6. 测试表达式只求值一次（符合 Go 语义，避免轮询重复求值）
(define eval-count 0)
(define (get-test-val)
  (set! eval-count (+ eval-count 1))
  "my-eval-val")

(define ch-once (make-chan 1))
(select
  ((chan-send! ch-once (get-test-val))
   #t))

(check eval-count => 1)
(check (chan-recv! ch-once) => "my-eval-val")

(check-report)
