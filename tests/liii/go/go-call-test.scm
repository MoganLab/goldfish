(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; go-call
;; 过程版本的 go，将函数与参数调度到后台 worker 线程并发执行。
;;
;; 语法
;; ----
;; (go-call fn arg ...)
;;
;; 参数
;; ----
;; fn : procedure
;; 顶层命名的函数。运行时通过 procedure-source 提取源码运输到后台 worker 线程。
;;
;; arg ... : any
;; 传递给 fn 的参数，必须为可序列化的数据类型。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 与 (go (fn arg ...)) 宏不同，go-call 是过程，可以与 apply 搭配用于动态构造参数列表。

;; 1. 基本调用与通道通信

(define (send-square ch x)
  (chan-send! ch (* x x) 2000)
) ;define

(define ch1 (make-chan 1))
(go-call send-square ch1 6)
(check (chan-recv! ch1 2000) => 36)

;; 2. 与 apply 配合进行动态调用

(define ch2 (make-chan 1))
(define args (list ch2 8))
(apply go-call send-square args)
(check (chan-recv! ch2 2000) => 64)

;; 3. 非过程参数抛出 type-error

(check-catch 'type-error (go-call 123 ch1))

(check-report)
