(import (scheme base)
  (liii check)
  (liii go)
  (liii string)
  (liii date-time)
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
  ;; 宽限期放宽到 5s：worker 线程启动（含 s7 会话初始化）在慢速 CI 上较慢，
  ;; 中止信号已缓冲，worker 就绪后即可结算
  (check (pi-app-abort-inflight! app 5000) => #t)
  ;; agent_settled 事件已把队列解锁
  (check (pi-queue-busy? q) => #f)
) ;let*

;; 3. busy 且 worker 不结算：宽限期结束后返回 #f（调用方据此强制退出）
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
       (start (current-monotonic-millis))
      ) ;
  (pi-queue-set-busy! q)
  (check (pi-app-abort-inflight! app 200) => #f)
  ;; 宽限期（200ms）后即返回：没有等待 worker 结算
  ;; （阈值放宽到 2s，兼容慢速 CI 机器；worker 永不结算，等待即为死等）
  (check-true (< (- (current-monotonic-millis) start) 2000))
  ;; 队列仍为 busy（未结算）
  (check (pi-queue-busy? q) => #t)
  ;; 中止信号已发出
  (check (chan-try-recv! (pi-app-abort-chan app) #f) => 'abort)
) ;let*

;; 4. 退出宽限期内不得派发新任务
;;    退出路径先中止在途任务，等待结算期间会调用 pi-app-poll-events!；
;;    agent_settled 会 pi-queue-finish-task! 把队列解锁，随后 settle! 调用
;;    pi-app-drain-queue!，若此时队列里还有排队消息，就会被再次派发给 worker，
;;    等于用户要求退出后又发起了新的 LLM 请求。
;;    正确行为：退出期间只中止、不派发；排队消息随进程退出而丢弃。
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (calls '())
       (app (make-pi-app :terminal vt
                :handler (lambda (a m) (set! calls (cons m calls)) #t)))
       (q (pi-app-queue app))
      ) ;
  (pi-queue-set-busy! q)
  ;; 队列中还有一条排队消息
  (pi-queue-enqueue! q "queued-msg")
  (go (settle-after-abort (pi-app-abort-chan app) (pi-app-event-chan app)))
  (check (pi-app-abort-inflight! app 5000) => #t)
  ;; 退出宽限期内不应派发任何新任务
  (check calls => '())
) ;let*

(check-report)
