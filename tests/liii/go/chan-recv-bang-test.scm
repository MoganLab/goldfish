(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; chan-recv!
;; 从通道接收一个值。
;;
;; 语法
;; ----
;; (chan-recv! ch)
;; (chan-recv! ch timeout-ms)
;; (chan-recv! ch timeout-ms default)
;;
;; 参数
;; ----
;; ch : channel
;; 源通道。
;;
;; timeout-ms : integer?
;; 可选。超时毫秒数。缺省时无限等待（Go 语义）。
;;
;; default : any
;; 可选。超时时返回的默认值，缺省为符号 timeout。
;;
;; 返回值
;; ----
;; any
;; 通道中的下一个值；超时返回 default；通道已关闭且读空时返回 eof-object。
;;
;; 说明
;; ----
;; 1. 无 timeout 形态等待时挂起协程而非阻塞物理线程：worker 会话内让出
;;    线程取下一个任务（任务级阻塞），主会话/调度器上下文内挂起 fiber
;;    等待跨线程唤醒。
;; 2. 带 timeout 形态为有限期阻塞等待。
;;
;; 错误处理
;; ----
;; ch 不是通道时抛出 type-error。

(define ch (make-chan 2))
(chan-send! ch 1)
(chan-send! ch 2)
(check (chan-recv! ch) => 1)
(check (chan-recv! ch) => 2)

;; 超时返回默认值
(check (chan-recv! ch 50 'empty) => 'empty)
(check (chan-recv! ch 50) => 'timeout)

;; 关闭后读空返回 eof-object

(define ch2 (make-chan 1))
(chan-send! ch2 'x)
(chan-close! ch2)
(check (chan-recv! ch2) => 'x)
(check (eof-object? (chan-recv! ch2)) => #t)

(check-catch 'type-error (chan-recv! "not-a-chan"))

;; 同会话 fiber 间经真 channel 通信：接收方挂起协程，发送方唤醒

(define ch3 (make-chan 1))
(define res3 #f)
(spawn-fiber (lambda () (set! res3 (chan-recv! ch3))))
(spawn-fiber (lambda () (chan-send! ch3 "intra-session")))
(fiber-scheduler-run!)
(check res3 => "intra-session")

;; 跨线程：worker 线程向真 channel 发送，唤醒主线程挂起的 fiber

(define ch4 (make-chan))
(define res4 #f)
(go (ch4)
  (g_msleep 50)
  (chan-send! ch4 "from-worker" 5000))
(spawn-fiber (lambda () (set! res4 (chan-recv! ch4))))
(fiber-scheduler-run!)
(check res4 => "from-worker")

(check-report)
