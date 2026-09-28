(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; fiber-send!
;; 向 fiber 通道发送一个值（无界缓冲，永不阻塞）。
;;
;; 语法
;; ----
;; (fiber-send! fch val)
;;
;; 参数
;; ----
;; fch : fiber-chan
;; 目标 fiber 通道。
;;
;; val : any
;; 要发送的值。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 若有挂起的接收者，直接唤醒并交付；否则追加到缓冲区。

(define fch (make-fiber-chan))

(define received '())
(spawn-fiber (lambda () (fiber-send! fch 1)))
(spawn-fiber (lambda () (fiber-send! fch 2)))
(spawn-fiber
  (lambda ()
    (set! received (cons (fiber-recv! fch) received))
    (set! received (cons (fiber-recv! fch) received))
  ) ;lambda
) ;spawn-fiber
(fiber-scheduler-run!)
(check (reverse received) => '(1 2))

(check-report)
