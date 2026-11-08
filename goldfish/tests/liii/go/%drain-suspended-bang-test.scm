;; %drain-suspended!
;; 消费 worker 会话中挂起的调度事件：存在真 channel 挂起或就绪 fiber 时进入
;; 调度器唤醒挂起协程，否则直接返回。
;;
;; 语法
;; ----
;; (%drain-suspended!)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; any
;; 需要进入调度器时返回调度器的返回值；无事可做时返回 #f。
;;
;; 说明
;; ----
;; 1. worker 空闲时 gate 事件被空唤醒，本接口负责把挂起的 fiber 拉回调度。
;; 2. 仅存在 fiber-chan 挂起时不进入调度器，避免误报 deadlock。

(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; 1. 空闲状态：既无真 channel 挂起也无就绪 fiber 时不做任何事

(check (%drain-suspended!) => #f)

;; 2. 存在就绪 fiber 时进入调度器并执行 fiber

(define log '())
(spawn-fiber (lambda () (set! log (cons 'ran log))))
(%drain-suspended!)
(check log => '(ran))

(check-report)
