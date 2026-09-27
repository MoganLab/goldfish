(import (liii check)
        (liii go)
        (scheme time))

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
;; 3. 底层为 C++ wait-set 事件驱动实现：分支就绪立即唤醒，timeout 精确到期。
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

;; 7. timeout 精度：100ms 超时的实际等待应在合理窗口内（轮询实现会有 1ms 粒度误差）
(define t0 (current-jiffy))
(select
  ((chan-recv! ch-empty v) v)
  (timeout 100 'timeout-ok))
(define elapsed-ms (* 1000.0 (/ (- (current-jiffy) t0) (jiffies-per-second))))
(check (< 99 elapsed-ms) => #t)
(check (< elapsed-ms 500) => #t)

;; 8. 已关闭通道的 recv 分支立即就绪（值为 eof-object）
(define ch-closed-sel (make-chan 1))
(chan-close! ch-closed-sel)
(define closed-hit #f)
(select
  ((chan-recv! ch-closed-sel v)
   (set! closed-hit v))
  (timeout 2000 'timeout))
(check (eof-object? closed-hit) => #t)

;; 9. select 的 send 分支遇到已关闭通道时报 value-error（与 chan-send! 一致）
(define ch-closed-send (make-chan 1))
(chan-close! ch-closed-send)
(check-catch 'value-error
  (select
    ((chan-send! ch-closed-send 1) #t)
    (timeout 2000 'timeout)))

;; 10. 无缓冲通道的 select rendezvous：worker 真实阻塞 recv 时 select send 就绪
(define ch-rv (make-chan))
(define ch-rv-done (make-chan 1))
(go (ch-rv ch-rv-done)
  (chan-send! ch-rv-done (chan-recv! ch-rv 5000) 5000))
(g_msleep 50)  ; 等待 worker 进入阻塞 recv
(define rv-selected #f)
(select
  ((chan-send! ch-rv "rv-val") (set! rv-selected #t))
  (timeout 2000 (set! rv-selected 'timeout)))
(check rv-selected => #t)
(check (chan-recv! ch-rv-done 2000) => "rv-val")

;; 11. 多分支同时就绪时选择其中一个（不假定顺序，只验证值正确）
(define ch-a (make-chan 1))
(define ch-b (make-chan 1))
(chan-send! ch-a "A")
(chan-send! ch-b "B")
(define multi-hit #f)
(select
  ((chan-recv! ch-a v) (set! multi-hit v))
  ((chan-recv! ch-b v) (set! multi-hit v)))
(check (if (member multi-hit (list "A" "B")) #t #f) => #t)

(check-report)
