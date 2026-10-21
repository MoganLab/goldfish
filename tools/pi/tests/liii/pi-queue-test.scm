(import (scheme base)
        (liii check)
        (liii pi agent queue))

(check-set-mode! 'report-failed)

;; -----------------------------------------------------------------------------
;; 测试项 1: 初始状态 - 队列为空且处于 idle 状态
;; -----------------------------------------------------------------------------
(let ((q (make-pi-message-queue)))
  (check (pi-message-queue? q) => #t)
  (check (pi-queue-empty? q) => #t)
  (check (pi-queue-length q) => 0)
  (check (pi-queue-idle? q) => #t)
  (check (pi-queue-busy? q) => #f)
  (check (pi-queue-items q) => '())
  ;; 空队列无法消费出队
  (check (pi-queue-dequeue-next! q) => #f))

;; -----------------------------------------------------------------------------
;; 测试项 2: 发送消息先入等待队列，出队消费后转为 busy 状态
;; -----------------------------------------------------------------------------
(let ((q (make-pi-message-queue)))
  ;; 用户发送消息 "hello agent" -> 首先进入等待队列
  (pi-queue-enqueue! q "hello agent")
  (check (pi-queue-empty? q) => #f)
  (check (pi-queue-length q) => 1)
  (check (pi-queue-items q) => '("hello agent"))
  (check (pi-queue-idle? q) => #t) ;; 此时任务尚未开始消费，调度器依然是 idle

  ;; 调度器出队消费这条消息
  (let ((consumed (pi-queue-dequeue-next! q)))
    (check consumed => "hello agent")
    (check (pi-queue-empty? q) => #t)
    (check (pi-queue-length q) => 0)
    (check (pi-queue-busy? q) => #t)  ;; 消费后转入 busy 状态
    (check (pi-queue-idle? q) => #f))

  ;; 当前任务正在执行中 (busy)，不允许并发提前出队
  (check (pi-queue-dequeue-next! q) => #f))

;; -----------------------------------------------------------------------------
;; 测试项 3: 前序任务未完成时，新消息持续排队；前序任务完成后按序消费
;; -----------------------------------------------------------------------------
(let ((q (make-pi-message-queue)))
  ;; 1. 第一个任务开始执行
  (pi-queue-enqueue! q "task 1")
  (check (pi-queue-dequeue-next! q) => "task 1")
  (check (pi-queue-busy? q) => #t)

  ;; 2. 在 task 1 执行期间，用户又发送了 task 2 与 task 3
  (pi-queue-enqueue! q "task 2")
  (pi-queue-enqueue! q "task 3")
  (check (pi-queue-length q) => 2)
  (check (pi-queue-items q) => '("task 2" "task 3"))
  ;; 任务 1 未完成，尝试 dequeue 依然返回 #f
  (check (pi-queue-dequeue-next! q) => #f)

  ;; 3. 前面的任务完成 (finish-task!)
  (pi-queue-finish-task! q)
  (check (pi-queue-idle? q) => #t)

  ;; 4. 调度消费下一个等待中的消息 -> task 2
  (check (pi-queue-dequeue-next! q) => "task 2")
  (check (pi-queue-busy? q) => #t)
  (check (pi-queue-length q) => 1)
  (check (pi-queue-items q) => '("task 3"))

  ;; 5. task 2 完成，调度消费 task 3
  (pi-queue-finish-task! q)
  (check (pi-queue-dequeue-next! q) => "task 3")
  (check (pi-queue-busy? q) => #t)
  (check (pi-queue-empty? q) => #t)

  ;; 6. task 3 完成，全部排空，恢复 idle 状态
  (pi-queue-finish-task! q)
  (check (pi-queue-idle? q) => #t)
  (check (pi-queue-busy? q) => #f)
  (check (pi-queue-dequeue-next! q) => #f))

;; -----------------------------------------------------------------------------
;; 测试项 4: 撤回等待队列中的所有消息 (Dequeue 回到输入框)
;; -----------------------------------------------------------------------------
(let ((q (make-pi-message-queue)))
  (pi-queue-enqueue! q "task A")
  (pi-queue-enqueue! q "task B")
  (check (pi-queue-length q) => 2)

  ;; 用户按快捷键撤回队列
  (let ((restored (pi-queue-restore-all! q)))
    (check restored => '("task A" "task B"))
    (check (pi-queue-empty? q) => #t)
    (check (pi-queue-length q) => 0)))

(check-report)
