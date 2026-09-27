(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; fiber-chan-recv!
;; 在 fiber 中从真 channel 接收值：挂起协程而非阻塞物理线程（M:N 阶段二）。
;;
;; 语法
;; ----
;; (fiber-chan-recv! ch)
;;
;; 参数
;; ----
;; ch : channel
;; 真 channel（make-chan 创建，可跨 worker 线程共享）。
;;
;; 返回值
;; ----
;; any
;; 通道中的下一个值；通道关闭且读空时返回 eof-object。
;;
;; 说明
;; ----
;; 1. 无数据时当前协程挂起，调度线程立即执行其他就绪协程或物理休眠等待。
;; 2. 唤醒来源可以是同会话 fiber、其他 worker 线程或 C++ 定时器。

;; 1. 同会话：fiber 间经真 channel 通信（缓冲通道）
;; 注意：fiber↔fiber 的无缓冲 rendezvous 暂不支持（双方都不会产生通道事件），
;; 会话内协程通信请用 fiber-chan 或缓冲通道；无缓冲通道用于 fiber↔worker 场景。
(define ch1 (make-chan 1))
(define res1 #f)
(spawn-fiber (lambda () (set! res1 (fiber-chan-recv! ch1))))
(spawn-fiber (lambda () (fiber-chan-send! ch1 "intra-session")))
(fiber-scheduler-run!)
(check res1 => "intra-session")

;; 2. 跨线程：worker 线程向真 channel 发送，唤醒主线程挂起的 fiber
(define ch2 (make-chan))
(define res2 #f)
(go (ch2)
  (g_msleep 50)
  (chan-send! ch2 "from-worker" 5000))
(spawn-fiber (lambda () (set! res2 (fiber-chan-recv! ch2))))
(fiber-scheduler-run!)
(check res2 => "from-worker")

;; 3. 缓冲通道：先有数据时立即返回不挂起
(define ch3 (make-chan 2))
(chan-send! ch3 "buffered")
(define res3 #f)
(spawn-fiber (lambda () (set! res3 (fiber-chan-recv! ch3))))
(fiber-scheduler-run!)
(check res3 => "buffered")

;; 4. 通道关闭时返回 eof-object
(define ch4 (make-chan))
(chan-close! ch4)
(define res4 #f)
(spawn-fiber (lambda () (set! res4 (fiber-chan-recv! ch4))))
(fiber-scheduler-run!)
(check (eof-object? res4) => #t)

(check-report)
