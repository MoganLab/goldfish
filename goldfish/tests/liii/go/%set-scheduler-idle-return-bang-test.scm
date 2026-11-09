;; %set-scheduler-idle-return!
;; 关闭调度器的空闲 park 策略：无可运行 fiber 时直接返回调用方，
;; 而不是在 gate 上挂起等待跨线程事件。
;;
;; 语法
;; ----
;; (%set-scheduler-idle-return!)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 供 worker 会话初始化时调用：worker 会话在调度器空闲时必须返回 C++ 层，
;; 以便继续取下一个任务；主会话保持默认的 park 策略。

(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; 1. 调用后调度器仍能正常执行就绪 fiber

(%set-scheduler-idle-return!)

(define log '())
(spawn-fiber (lambda () (set! log (cons 'ran log))))
(fiber-scheduler-run!)
(check log => '(ran))

;; 2. 空闲返回模式：只剩挂在真 channel 上的 fiber 时调度器立即返回，
;;    不再 park 等待跨线程事件

(define ch (make-chan 1))
(spawn-fiber (lambda () (chan-recv! ch)))
(check (fiber-scheduler-run!) => #t)

(check-report)
