(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; fiber-scheduler-run!
;; 启动协程调度器，运行直到所有就绪 fiber 执行完毕。
;;
;; 语法
;; ----
;; (fiber-scheduler-run!)
;;
;; 返回值
;; ----
;; #t
;;
;; 错误处理
;; ----
;; 所有 fiber 都阻塞在通道上（死锁）时抛出 deadlock 错误，
;; 报错后调度器状态复位，可再次运行。

;; 大规模协程压测：1 万个协程
(define N 10000)
(define count-val 0)
(let loop ((i 0))
  (if (< i N)
      (begin
        (spawn-fiber (lambda () (set! count-val (+ count-val 1))))
        (loop (+ i 1)))))
(fiber-scheduler-run!)
(check count-val => N)

;; 死锁检测
(define fch (make-fiber-chan))
(spawn-fiber (lambda () (fiber-recv! fch)))
(check-catch 'deadlock (fiber-scheduler-run!))

(check-report)
