;; pi-app-submit-cancelling
;; 测试在任务被 ESC 中止（cancelling 状态）期间用户立即提交新消息：
;; 新消息不被作为旧任务的 steering 吞没，而是排入等待队列，在 abort 结算后无缝接续执行。
;;
;; 语法
;; ----
;; (pi-app-submit! app text)
;;
;; 参数
;; ----
;; app : pi-app?
;; 处于 cancelling 状态的交互式应用实例。
;; text : string
;; 用户新提交的输入文本。
;;
;; 返回值
;; ------
;; any
;; 执行消息安全入队并在任务结算后自动接续执行。

(import (scheme base)
  (liii check)
  (liii go)
  (liii tui)
  (liii pi agent queue)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive app-queue)
  (liii pi code modes interactive run)
) ;import

(check-set-mode! 'report-failed)

;; 1. cancelling 状态下提交新消息：直接进入等待队列，不作为 steering 注入
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (submitted #f)
       (app (make-pi-app :terminal vt :handler (lambda (a m) (set! submitted m))))
       (q (pi-app-queue app))
       (ed (pi-app-editor app))
      ) ;
  (pi-queue-set-busy! q)
  (pi-queue-set-cancelling! q)
  (check-true (pi-queue-cancelling? q))
  (check-false (pi-queue-busy? q))
  (pi-app-submit! app "新任务 A")
  ;; 验证：没有作为 steering 丢失，而是安全保存在等待队列中
  (check (%pi-app-steering-pending app) => '())
  (check (pi-queue-length q) => 1)
  (check (editor-text ed) => "")
  (check submitted => #f)
  ;; 模拟 worker 发送 agent_settled 结算：解锁队列并自动接续派发
  (pi-queue-finish-task! q)
  (check (pi-app-drain-queue! app) => #t)
  (check submitted => "新任务 A")
  (check (pi-queue-length q) => 0)
) ;let*

;; 2. ESC 处理触发 cancelling，随后提交新消息：不作为 steering，结算后自动接续执行
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (submitted #f)
       (app (make-pi-app :terminal vt :handler (lambda (a m) (set! submitted m))))
       (q (pi-app-queue app))
       (ed (pi-app-editor app))
       (ev-ch (pi-app-event-chan app))
      ) ;
  ;; 1) 模拟正在执行任务
  (pi-queue-set-busy! q)
  ;; 2) 用户按 ESC 键中止
  (check (pi-app-handle-escape! app) => #t)
  ;; 验证：ESC 发出了 abort 信号且队列转入 cancelling 状态
  (check (chan-try-recv! (pi-app-abort-chan app) #f) => 'abort)
  (check-true (pi-queue-cancelling? q))
  (check-false (pi-queue-busy? q))
  ;; 3) 用户紧接着提交新任务
  (pi-app-submit! app "新任务 B")
  ;; 验证：新任务安全进入等待队列，不作为旧任务 steering 丢失
  (check (%pi-app-steering-pending app) => '())
  (check (pi-queue-length q) => 1)
  (check (editor-text ed) => "")
  (check submitted => #f)
  ;; 4) 后台 worker 完成中止结算，回送 agent_settled 事件
  (chan-send! ev-ch '(agent_settled #t) 0)
  (pi-app-poll-events! app)
  ;; 验证：poll-events 消费 agent_settled 后，自动触发 drain-queue 接续执行新任务
  (check submitted => "新任务 B")
  (check (pi-queue-length q) => 0)
) ;let*

(check-report)
