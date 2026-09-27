(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; spawn-fiber
;; 创建一个轻量级用户态协程（fiber），加入就绪队列。
;;
;; 语法
;; ----
;; (spawn-fiber thunk)
;;
;; 参数
;; ----
;; thunk : procedure
;; 协程体，零参数过程。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 1. spawn 只入队不执行，需调用 fiber-scheduler-run! 启动调度。
;; 2. fiber 是协作式调度：只有 fiber-yield! 或 fiber 通道操作才会切换。

(define log '())
(spawn-fiber (lambda () (set! log (cons 'a log))))
(spawn-fiber (lambda () (set! log (cons 'b log))))
(fiber-scheduler-run!)
(check (reverse log) => '(a b))

(check-report)
