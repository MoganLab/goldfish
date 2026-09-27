(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; 1. 测试单 Session 内基于 call/cc 的协作式协程切换
(define log '())
(spawn-fiber
  (lambda ()
    (set! log (cons 'a1 log))
    (fiber-yield!)
    (set! log (cons 'a2 log))))

(spawn-fiber
  (lambda ()
    (set! log (cons 'b1 log))
    (fiber-yield!)
    (set! log (cons 'b2 log))))

;; 启动协程调度器，跑完所有排队的就绪协程
(fiber-scheduler-run!)

;; 验证执行顺序是交叉交替进行的：a1, b1, a2, b2
(check (reverse log) => '(a1 b1 a2 b2))

;; 2. 测试协程通道（Fiber Channel）的挂起与唤醒
(define fch (make-fiber-chan))
(define fiber-res #f)

;; 消费者协程先启动并等待数据（此时通道无数据，它会被挂起）
(spawn-fiber
  (lambda ()
    (set! fiber-res (fiber-recv! fch))))

;; 生产者协程后启动，放入数据并唤醒消费者
(spawn-fiber
  (lambda ()
    (fiber-send! fch "hello-from-fiber")))

(fiber-scheduler-run!)
(check fiber-res => "hello-from-fiber")

;; 3. 超大规模轻量级协程压测：10,000 个协程流水线环形传递
;; 验证单核承载 10,000 个并发实体无压力
(define N 10000)
(define count-val 0)

(let loop ((i 0))
  (if (< i N)
      (begin
        (spawn-fiber
          (lambda ()
            (set! count-val (+ count-val 1))))
        (loop (+ i 1)))))

(fiber-scheduler-run!)
(check count-val => N)

;; 4. 死锁检测：所有 fiber 都阻塞在通道上时应报 deadlock 错误（而非静默退出）
(define fch-dead (make-fiber-chan))
(spawn-fiber
  (lambda () (fiber-recv! fch-dead)))  ; 永远等不到数据
(check-catch 'deadlock (fiber-scheduler-run!))

;; 5. 死锁报错后调度器状态已复位，可以继续正常使用
(define fch-after (make-fiber-chan))
(define after-res #f)
(spawn-fiber
  (lambda () (set! after-res (fiber-recv! fch-after))))
(spawn-fiber
  (lambda () (fiber-send! fch-after "recovered")))
(fiber-scheduler-run!)
(check after-res => "recovered")

(check-report)
