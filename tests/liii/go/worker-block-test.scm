(import (liii check) (liii go) (liii time))

(check-set-mode! 'report-failed)

;; 复现 worker 线程级阻塞：任务阻塞在无数据的 channel 上时，
;; 整个 OS 线程连同后续排队任务一起停摆（线程级阻塞而非任务级阻塞）。
;;
;; 场景：先派 n（= go-worker-count）个阻塞任务占满所有 worker，
;; 再派一个正常任务。按 Go 的任务级阻塞语义，正常任务应能完成；
;; 线程级阻塞下正常任务永远排队，主线程只能超时。

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
