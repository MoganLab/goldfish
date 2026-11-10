;; %run-worker-task
;; worker 会话的任务执行入口：把任务包成 fiber 推进就绪队列并运行调度器，
;; 调度器错误（如死锁）由错误处理过程兜底。
;;
;; 语法
;; ----
;; (%run-worker-task thunk err-handler)
;;
;; 参数
;; ----
;; thunk : procedure
;; 零参数任务体，在 fiber 中执行。
;; err-handler : procedure
;; 错误处理过程，接收错误标识与错误参数列表两个参数。
;;
;; 返回值
;; ----
;; any
;; 任务正常结束时返回调度器的返回值；任务抛出异常时返回 err-handler 的返回值。
;;
;; 说明
;; ----
;; 1. 调度器空闲时直接返回，挂在真 channel 上的 fiber 留在会话中，
;;    由后续任务再次进入调度器时经 gate 事件唤醒。
;; 2. 必须是源码定义而非 C 层拼装的同构表达式：C 拼装表达式走求值器通用路径，
;;    其 continuation 栈布局与源码路径不同，收尾逃逸恢复时参数会被误传入。

(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; 1. 正常路径：任务体被执行

(define ran #f)
(%run-worker-task (lambda () (set! ran #t)) (lambda (tag args) 'unreachable))
(check ran => #t)

;; 2. 异常路径：错误处理过程收到原始错误标识

(define caught #f)
(%run-worker-task (lambda () (error 'test-error "boom"))
  (lambda (tag args) (set! caught tag))
) ;%run-worker-task
(check caught => 'test-error)

;; 3. 死锁路径：任务内 fiber 全部阻塞时抛 deadlock，由错误处理过程兜底
;; （放在最后：死锁逃逸会跳过调度器的恢复代码）

(define caught-deadlock #f)
(%run-worker-task
  (lambda ()
    (spawn-fiber (lambda () (fiber-recv! (make-fiber-chan))))
    (fiber-scheduler-run!)
  ) ;lambda
  (lambda (tag args) (set! caught-deadlock tag))
) ;%run-worker-task
(check caught-deadlock => 'deadlock)

(check-report)
