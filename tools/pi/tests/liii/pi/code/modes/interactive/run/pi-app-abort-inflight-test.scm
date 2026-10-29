(import (scheme base)
  (liii check)
  (liii go)
  (liii string)
  (liii tui)
  (liii pi agent queue)
  (liii pi code modes interactive app)
  (liii pi code modes interactive run)
) ;import

(check-set-mode! 'report)

;; 退出前中止在途任务（修复 working 状态下 Ctrl+D 退出全屏后卡住）：
;;   worker 线程正阻塞在 LLM 流式 HTTP 请求上，而进程退出会 join go 线程池
;;   （GoThreadPool 析构 → shutdown → join），不主动中止就会等到 provider
;;   超时（默认 300s）才返回 shell 提示符。
;;   这里验证中止信号的发送与结算等待：已结算 → #t，超时未结算 → #f。

;; 模拟 worker：收到 abort 信号后回送 agent_settled 事件
(define (settle-after-abort abort-ch event-ch)
  (chan-recv! abort-ch 2000)
  (chan-send! event-ch (list 'agent_settled #t) 1000)
  #t
) ;define

;; 1. 空闲（队列非 busy）：直接返回 #t，且不发送中止信号
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  (check (pi-app-abort-inflight! app 50) => #t)
  (check (chan-try-recv! (pi-app-abort-chan app) #f) => #f)
) ;let*

;; 2. busy 且 worker 能结算：发出中止信号并等到队列回到 idle
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (pi-queue-set-busy! q)
  (go (settle-after-abort (pi-app-abort-chan app) (pi-app-event-chan app)))
  (check (pi-app-abort-inflight! app 2000) => #t)
  ;; agent_settled 事件已把队列解锁
  (check (pi-queue-busy? q) => #f)
) ;let*

;; 3. busy 且 worker 不结算：宽限期结束后返回 #f（调用方据此强制退出）
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (pi-queue-set-busy! q)
  (check (pi-app-abort-inflight! app 50) => #f)
  ;; 队列仍为 busy（未结算）
  (check (pi-queue-busy? q) => #t)
  ;; 中止信号已发出
  (check (chan-try-recv! (pi-app-abort-chan app) #f) => 'abort)
) ;let*

(check-report)
