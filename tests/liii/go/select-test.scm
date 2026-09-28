(import (liii check) (liii go) (scheme time))

(check-set-mode! 'report-failed)

;; select
;; 多路复用：等待多个通道操作中第一个就绪者（Go select 语义）。
;;
;; 语法
;; ----
;; (select
;;   ((chan-recv! ch) => proc)
;;   ((chan-recv! ch var) body ...)
;;   ((chan-send! ch expr) body ...)
;;   (timeout ms body ...)
;;   (else body ...))
;;
;; 子句
;; ----
;; ((chan-recv! ch) => proc) : 通道可读时，将接收到的值传给单参过程 proc
;; ((chan-recv! ch var) body ...) : 通道可读时，将值绑定到 var 并执行 body
;; ((chan-send! ch expr) body ...) : 通道可写时，发送 expr 并执行 body
;; (timeout ms body ...) : 超过 ms 毫秒仍无就绪分支时执行 body
;; (else body ...) : 无阻塞立即执行（与 timeout 互斥）
;;
;; 说明
;; ----
;; 1. 各分支的通道与发送表达式只在进入 select 时求值一次（Go 语义）。
;; 2. 多个分支同时就绪时伪随机选择（符合 Go 规范的公平性，避免后序分支饿死）。
;; 3. 底层为 C++ wait-set 事件驱动实现：分支就绪立即唤醒，timeout 精确到期。
;;
;; 错误处理
;; ----
;; else 与 timeout 同时出现、或同一类子句重复出现时抛出 syntax-error。

;; 1. 测试 else 非阻塞分支：通道为空时立即走 else

(define ch1 (make-chan 1))

(define ch2 (make-chan 1))

(define hit-else #f)
(select ((chan-recv! ch1 v) (set! hit-else 'ch1))
 ((chan-recv! ch2 v) (set! hit-else 'ch2))
 (else (set! hit-else #t))
) ;select

(check hit-else => #t)

;; 2. 测试 chan-recv! 就绪分支
(chan-send! ch1 "hello-select")

(define recv-val #f)
(select ((chan-recv! ch1 v) (set! recv-val v))
 ((chan-recv! ch2 v) (set! recv-val "wrong"))
 (else (set! recv-val "else"))
) ;select

(check recv-val => "hello-select")

;; 3. 测试 chan-send! 就绪分支

(define ch-send (make-chan 1))

(define sent-ok #f)

(select ((chan-send! ch-send 999) (set! sent-ok #t)) (else (set! sent-ok #f)))

(check sent-ok => #t)
(check (chan-recv! ch-send) => 999)

;; 4. 测试 timeout 超时分支

(define timeout-hit #f)

(define ch-empty (make-chan 1))

(select ((chan-recv! ch-empty v) (set! timeout-hit 'recv))
  (timeout 50 (set! timeout-hit 'timeout))
) ;select

(check timeout-hit => 'timeout)

;; 5. 测试多核并发下的 select：后台协程延迟发送，主协程 select 阻塞命中

(define ch-async (make-chan 1))
(go (ch-async)
  ;; 稍微工作一下后发送数据
  (letrec ((fib (lambda (x) (if (<= x 1) x (+ (fib (- x 1)) (fib (- x 2)))))))
    (fib 30)
  ) ;letrec
  (chan-send! ch-async "async-ready")
) ;go

(define async-result #f)
(select ((chan-recv! ch-async msg) (set! async-result msg))
  (timeout 2000 (set! async-result 'timed-out))
) ;select

(check async-result => "async-ready")

;; 6. 测试表达式只求值一次（符合 Go 语义，避免轮询重复求值）

(define eval-count 0)

(define (get-test-val)
  (set! eval-count (+ eval-count 1))
  "my-eval-val"
) ;define

(define ch-once (make-chan 1))
(select ((chan-send! ch-once (get-test-val)) #t))

(check eval-count => 1)
(check (chan-recv! ch-once) => "my-eval-val")

;; 7. timeout 精度：100ms 超时的实际等待应在合理窗口内（轮询实现会有 1ms 粒度误差）

(define t0 (current-jiffy))
(select ((chan-recv! ch-empty v) v) (timeout 100 'timeout-ok))

(define elapsed-ms (* 1000.0 (/ (- (current-jiffy) t0) (jiffies-per-second))))
(check (< 99 elapsed-ms) => #t)
(check (< elapsed-ms 500) => #t)

;; 8. 已关闭通道的 recv 分支立即就绪（值为 eof-object）

(define ch-closed-sel (make-chan 1))
(chan-close! ch-closed-sel)

(define closed-hit #f)
(select ((chan-recv! ch-closed-sel v) (set! closed-hit v))
  (timeout 2000 'timeout)
) ;select
(check (eof-object? closed-hit) => #t)

;; 9. select 的 send 分支遇到已关闭通道时报 value-error（与 chan-send! 一致）

(define ch-closed-send (make-chan 1))
(chan-close! ch-closed-send)
(check-catch 'value-error
  (select ((chan-send! ch-closed-send 1) #t) (timeout 2000 'timeout))
) ;check-catch

;; 10. 无缓冲通道的 select rendezvous：worker 真实阻塞 recv 时 select send 就绪

(define ch-rv (make-chan))

(define ch-rv-done (make-chan 1))
(go (ch-rv ch-rv-done) (chan-send! ch-rv-done (chan-recv! ch-rv 5000) 5000))
(g_msleep 50)

(define rv-selected #f)
(select ((chan-send! ch-rv "rv-val") (set! rv-selected #t))
  (timeout 2000 (set! rv-selected 'timeout))
) ;select
(check rv-selected => #t)
(check (chan-recv! ch-rv-done 2000) => "rv-val")

;; 11. 多分支同时就绪时选择其中一个（不假定顺序，只验证值正确）

(define ch-a (make-chan 1))

(define ch-b (make-chan 1))
(chan-send! ch-a "A")
(chan-send! ch-b "B")

(define multi-hit #f)
(select ((chan-recv! ch-a v) (set! multi-hit v))
 ((chan-recv! ch-b v) (set! multi-hit v))
) ;select
(check (if (member multi-hit (list "A" "B")) #t #f) => #t)

;; 12. 测试 ((chan-recv! ch) => proc) 语法

(define ch-arrow1 (make-chan 1))

(define ch-arrow2 (make-chan 1))
(chan-send! ch-arrow1 100)

(define (arrow-handler val)
  (+ val 23)
) ;define

(define arrow-res
  (select ((chan-recv! ch-arrow1) => arrow-handler)
   ((chan-recv! ch-arrow2) => (lambda (v) (* v 2)))
   (else 'arrow-else)
  ) ;select
) ;define

(check arrow-res => 123)

;; 13. 测试 ((chan-recv! ch) => proc) 未就绪时命中 else

(define arrow-else-res
  (select ((chan-recv! ch-arrow2) => arrow-handler) (else 'hit-arrow-else))
) ;define

(check arrow-else-res => 'hit-arrow-else)

;; 14. 测试 ((timeout ms) => proc) 形式：就绪且传递实际毫秒数

(define timeout-arrow-ms #f)

(define ch-empty-arrow (make-chan))
(select ((chan-recv! ch-empty-arrow) => (lambda (v) (set! timeout-arrow-ms 'recv)))
 ((timeout 50) => (lambda (ms) (set! timeout-arrow-ms ms)))
) ;select
(check (integer? timeout-arrow-ms) => #t)
(check (>= timeout-arrow-ms 45) => #t)

;; 15. 测试 ((timeout ms) body ...) 块形式

(define timeout-block-hit #f)
(select ((chan-recv! ch-empty-arrow) => (lambda (v) #f))
 ((timeout 50) (set! timeout-block-hit #t) 'block-ret)
) ;select
(check timeout-block-hit => #t)

;; 16. 测试 (timeout ms => proc) 形式

(define timeout-flat-arrow-ms #f)
(select ((chan-recv! ch-empty-arrow) => (lambda (v) #f))
  (timeout 50 => (lambda (ms) (set! timeout-flat-arrow-ms ms)))
) ;select
(check (integer? timeout-flat-arrow-ms) => #t)
(check (>= timeout-flat-arrow-ms 45) => #t)

;; 17. 错误路径测试：多个 else、多个 timeout 或 else 与 timeout 混用
(check-catch 'syntax-error
  (eval '(select ((chan-recv! ch-arrow1) => arrow-handler) (else 1) (else 2)))
) ;check-catch

(check-catch 'syntax-error
  (eval '(select ((chan-recv! ch-arrow1) => arrow-handler)
           (timeout 100 1)
           (else 2))
  ) ;eval
) ;check-catch

(check-catch 'syntax-error
  (eval '(select ((chan-recv! ch-arrow1) => arrow-handler)
           ((timeout 100) 1)
           (else 2))
  ) ;eval
) ;check-catch

(check-catch 'syntax-error
  (eval '(select ((chan-recv! ch-arrow1) => arrow-handler)
           ((timeout 100) 1)
           ((timeout 200) 2))
  ) ;eval
) ;check-catch

(check-catch 'syntax-error
  (eval '(select ((chan-recv! ch-arrow1) => arrow-handler)
           ((timeout 100) => 1 2))
  ) ;eval
) ;check-catch

;; 18. 测试多 recv 分支同时就绪时的伪随机公平性（避免按顺序扫描导致后序分支饿死）

(define ch-fair-a (make-chan 1))

(define ch-fair-b (make-chan 1))
(chan-send! ch-fair-a "A")
(chan-send! ch-fair-b "B")

(define fair-recv-count-a 0)

(define fair-recv-count-b 0)

(let loop
  ((i 0))
  (when (< i 200)
    (select ((chan-recv! ch-fair-a v)
             (set! fair-recv-count-a (+ fair-recv-count-a 1))
             (chan-send! ch-fair-a "A")
            ) ;
     ((chan-recv! ch-fair-b v)
      (set! fair-recv-count-b (+ fair-recv-count-b 1))
      (chan-send! ch-fair-b "B")
     ) ;
    ) ;select
    (loop (+ i 1))
  ) ;when
) ;let

;; 200 次采样中，两个分支均应被充分选中（各在 40~160 之间），不能出现一侧为 0 的饥饿现象
(check (> fair-recv-count-a 40) => #t)
(check (> fair-recv-count-b 40) => #t)

;; 19. 测试 recv 与 send 混合同时就绪时的伪随机公平性（避免读永远优先于写）

(define ch-fair-r (make-chan 1))

(define ch-fair-w (make-chan 1))
(chan-send! ch-fair-r "R")
;; ch-fair-r 满，可读
;; ch-fair-w 为空，可写

(define fair-mix-recv-count 0)

(define fair-mix-send-count 0)

(let loop
  ((i 0))
  (when (< i 200)
    (select ((chan-recv! ch-fair-r v)
             (set! fair-mix-recv-count (+ fair-mix-recv-count 1))
             (chan-send! ch-fair-r "R")
            ) ;
     ((chan-send! ch-fair-w "W")
      (set! fair-mix-send-count (+ fair-mix-send-count 1))
      (chan-recv! ch-fair-w)
     ) ;
    ) ;select
    (loop (+ i 1))
  ) ;when
) ;let

;; 200 次采样中，recv 与 send 均应被充分选中（避免 recv 独占）
(check (> fair-mix-recv-count 40) => #t)
(check (> fair-mix-send-count 40) => #t)

(check-report)
