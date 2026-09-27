(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; fiber-recv!
;; 从 fiber 通道接收一个值；无数据时挂起当前协程（不占用物理线程）。
;;
;; 语法
;; ----
;; (fiber-recv! fch)
;;
;; 参数
;; ----
;; fch : fiber-chan
;; 源 fiber 通道。
;;
;; 返回值
;; ----
;; any
;; 通道中的下一个值。

;; 消费者先挂起，生产者后唤醒
(define fch (make-fiber-chan))
(define res #f)
(spawn-fiber (lambda () (set! res (fiber-recv! fch))))
(spawn-fiber (lambda () (fiber-send! fch "hello")))
(fiber-scheduler-run!)
(check res => "hello")

(check-report)
