;; go
;; worker 线程级阻塞的回归测试：任务阻塞在无数据的通道上时，整个 OS 线程
;; 连同后续排队任务一起停摆（线程级阻塞而非任务级阻塞）。本文件验证任务级
;; 阻塞语义下后续任务仍能完成。
;;
;; 语法
;; ----
;; (go (fn arg ...))
;;
;; 参数
;; ----
;; fn : procedure
;; 顶层命名的函数，源码被运输到后台 worker 会话执行。
;; arg ... : any
;; 传递给 fn 的参数，必须为可序列化的数据类型。
;;
;; 返回值
;; ----
;; unspecified

(import (liii check) (liii go) (liii time))

(check-set-mode! 'report-failed)

(define n (go-worker-count))

(define block-ch (make-chan 0))

(define (block-task ch)
  (chan-recv! ch)
) ;define

(define (send-ok-task ch)
  (chan-send! ch 'ok)
) ;define

;; 1. 对照组：n-1 个阻塞任务，仍留有空闲 worker，正常任务应能完成
(let loop
  ((i 0))
  (if (< i (- n 1)) (begin (go (block-task block-ch)) (loop (+ i 1))))
) ;let
(sleep 0.2)

(define res1 (make-chan 1))
(go (send-ok-task res1))
(check (chan-recv! res1 3000 'timeout) => 'ok)

;; 2. 正式组：再派 1 个阻塞任务占满最后一个 worker，正常任务应仍能完成。
;; 任务级阻塞语义下它必须完成；线程级阻塞下它永远排队。
(go (block-task block-ch))
(sleep 0.2)

(define res2 (make-chan 1))
(go (send-ok-task res2))
(check (chan-recv! res2 3000 'timeout) => 'ok)

;; 清理：唤醒所有阻塞 worker，保证测试进程能正常退出
(let loop
  ((i 0))
  (if (< i n) (begin (chan-send! block-ch 'wake) (loop (+ i 1))))
) ;let

(check-report)
