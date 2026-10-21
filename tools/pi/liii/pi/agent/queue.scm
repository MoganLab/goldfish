;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi agent queue)
  (export make-pi-message-queue pi-message-queue? pi-queue-empty?
    pi-queue-length pi-queue-idle? pi-queue-busy? pi-queue-items
    pi-queue-enqueue! pi-queue-dequeue-next! pi-queue-finish-task!
    pi-queue-set-busy! pi-queue-restore-all!
  ) ;export
  (import (scheme base))
  (begin

    ;; 消息等待队列与调度状态机
    ;; status: 'idle（空闲） 或 'busy（有任务正在执行）
    ;; items:  等待被消费的消息列表（FIFO 队列）
    (define-record-type <pi-message-queue>
      (%make-pi-message-queue status items)
      pi-message-queue?
      (status %pi-queue-status %pi-queue-status-set!)
      (items %pi-queue-items %pi-queue-items-set!)
    ) ;define-record-type

    (define (make-pi-message-queue)
      (%make-pi-message-queue 'idle '())
    ) ;define

    (define (pi-queue-empty? q)
      (null? (%pi-queue-items q))
    ) ;define

    (define (pi-queue-length q)
      (length (%pi-queue-items q))
    ) ;define

    (define (pi-queue-idle? q)
      (eq? (%pi-queue-status q) 'idle)
    ) ;define

    (define (pi-queue-busy? q)
      (eq? (%pi-queue-status q) 'busy)
    ) ;define

    (define (pi-queue-items q)
      (%pi-queue-items q)
    ) ;define

    ;; 发送消息：无论当前是否 busy，都先进入等待队列
    (define (pi-queue-enqueue! q msg)
      (let ((current (%pi-queue-items q)))
        (%pi-queue-items-set! q (append current (list msg)))
        (pi-queue-length q)
      ) ;let
    ) ;define

    ;; 调度出队消费：
    ;; 只有当前不是 busy 且队列非空时，才能消费队首消息并转入 busy 状态
    (define (pi-queue-dequeue-next! q)
      (cond ((pi-queue-busy? q) #f)
            ((pi-queue-empty? q) (%pi-queue-status-set! q 'idle) #f)
            (else
              (let* ((items (%pi-queue-items q)) (next-msg (car items)))
                (%pi-queue-items-set! q (cdr items))
                (%pi-queue-status-set! q 'busy)
                next-msg
              ) ;let*
            ) ;else
      ) ;cond
    ) ;define

    ;; 前序任务完成通知：解除 busy 锁定，恢复为 idle
    (define (pi-queue-finish-task! q)
      (%pi-queue-status-set! q 'idle)
    ) ;define

    ;; 标记队列为 busy 状态
    (define (pi-queue-set-busy! q)
      (%pi-queue-status-set! q 'busy)
    ) ;define

    ;; 快捷键撤回队列：排空所有未被消费的消息并返回
    (define (pi-queue-restore-all! q)
      (let ((all (%pi-queue-items q)))
        (%pi-queue-items-set! q '())
        all
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
