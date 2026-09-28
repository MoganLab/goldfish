(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; fiber-chan-send!
;; 在 fiber 中向真 channel 发送值：挂起协程而非阻塞物理线程（M:N 阶段二）。
;;
;; 语法
;; ----
;; (fiber-chan-send! ch val)
;;
;; 参数
;; ----
;; ch : channel
;; 真 channel。
;;
;; val : any
;; 要发送的值（序列化规则同 chan-send!）。
;;
;; 返回值
;; ----
;; #t
;;
;; 错误处理
;; ----
;; 向已关闭的通道发送抛出 value-error。

;; 1. 缓冲通道：fiber 间发送接收（无缓冲的 fiber↔fiber rendezvous 暂不支持，
;; 见 fiber-chan-recv! 文档说明）

(define ch1 (make-chan 1))

(define sent #f)
(spawn-fiber (lambda () (fiber-chan-send! ch1 "rv") (set! sent #t)))
(spawn-fiber (lambda ()
               ;; 同会话另一个 fiber 接收（真实阻塞 recv）
               (fiber-chan-recv! ch1)
             ) ;lambda
) ;spawn-fiber
(fiber-scheduler-run!)
(check sent => #t)

;; 2. 缓冲满时挂起，另一 fiber 消费后唤醒

(define ch2 (make-chan 1))
(chan-send! ch2 "old")

(define order '())
(spawn-fiber (lambda () (fiber-chan-send! ch2 "new") (set! order (cons 'send-done order)))
) ;spawn-fiber
(spawn-fiber (lambda () (fiber-chan-recv! ch2) (set! order (cons 'recv-done order)))
) ;spawn-fiber
(fiber-scheduler-run!)
(check (car order) => 'send-done)

;; 3. 向已关闭通道发送抛 value-error

(define ch3 (make-chan 1))
(chan-close! ch3)
(check-catch 'value-error (fiber-chan-send! ch3 1))

(check-report)
