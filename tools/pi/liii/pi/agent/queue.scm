;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi agent queue)
  (export make-pi-message-queue pi-message-queue? pi-queue-empty?
    pi-queue-length pi-queue-idle? pi-queue-busy? pi-queue-items
    pi-queue-enqueue! pi-queue-dequeue-next! pi-queue-finish-task!
    pi-queue-set-busy! pi-queue-restore-all! make-pi-pending-queue
    pi-pending-queue? pi-pending-mode pi-pending-mode-set! pi-pending-empty?
    pi-pending-length pi-pending-items pi-pending-enqueue! pi-pending-peek
    pi-pending-drain! pi-pending-clear!
  ) ;export
  (import (scheme base) (liii queue) (liii error))
  (begin

    ;; 消息等待队列与调度状态机
    ;; status: 'idle（空闲） 或 'busy（有任务正在执行）
    ;; items:  等待被消费的消息队列（内部复用 SRFI-117 list-queue，FIFO）
    (define-record-type <pi-message-queue>
      (%make-pi-message-queue status items)
      pi-message-queue?
      (status %pi-queue-status %pi-queue-status-set!)
      (items %pi-queue-items)
    ) ;define-record-type

    (define (make-pi-message-queue)
      (%make-pi-message-queue 'idle (make-list-queue '()))
    ) ;define

    (define (pi-queue-empty? q)
      (list-queue-empty? (%pi-queue-items q))
    ) ;define

    (define (pi-queue-length q)
      (length (list-queue-list (%pi-queue-items q)))
    ) ;define

    (define (pi-queue-idle? q)
      (eq? (%pi-queue-status q) 'idle)
    ) ;define

    (define (pi-queue-busy? q)
      (eq? (%pi-queue-status q) 'busy)
    ) ;define

    (define (pi-queue-items q)
      (list-queue-list (%pi-queue-items q))
    ) ;define

    ;; 发送消息：无论当前是否 busy，都先进入等待队列
    (define (pi-queue-enqueue! q msg)
      (list-queue-add-back! (%pi-queue-items q) msg)
      (pi-queue-length q)
    ) ;define

    ;; 调度出队消费：
    ;; 只有当前不是 busy 且队列非空时，才能消费队首消息并转入 busy 状态
    (define (pi-queue-dequeue-next! q)
      (cond ((pi-queue-busy? q) #f)
            ((pi-queue-empty? q) #f)
            (else (%pi-queue-status-set! q 'busy)
              (list-queue-remove-front! (%pi-queue-items q))
            ) ;else
      ) ;cond
    ) ;define

    ;; 前序任务完成通知：解除 busy 锁定，恢复为 idle
    (define (pi-queue-finish-task! q)
      (%pi-queue-status-set! q 'idle)
    ) ;define

    ;; 标记队列为 busy 状态
    ;; 仅供测试模拟 busy 态使用；正常流程由 pi-queue-dequeue-next!
    ;; 在消费消息时原子地转入 busy，保证“busy 必有对应消费”这一不变量
    (define (pi-queue-set-busy! q)
      (%pi-queue-status-set! q 'busy)
    ) ;define

    ;; 快捷键撤回队列：排空所有未被消费的消息并返回
    (define (pi-queue-restore-all! q)
      (list-queue-remove-all! (%pi-queue-items q))
    ) ;define

    ;; steering / follow-up 待处理队列（对齐上游 pi 的 PendingMessageQueue）
    ;; mode: 'one-at-a-time（每轮只投递队首一条，默认）
    ;;    或 'all（一次性投递全部）
    ;; items: FIFO 消息队列
    ;; 与 <pi-message-queue> 的分工：后者负责 TUI 主线程的整轮任务调度
    ;; （idle/busy 状态机），本类型只表达"运行中/运行后注入消息"的投递粒度
    (define-record-type <pi-pending-queue>
      (%make-pi-pending-queue mode items)
      pi-pending-queue?
      (mode %pi-pending-queue-mode %pi-pending-queue-mode-set!)
      (items %pi-pending-queue-items)
    ) ;define-record-type

    (define (%pi-pending-check-mode mode)
      (if (memq mode '(one-at-a-time all))
        mode
        (type-error (string-append "pi-pending-mode-set!: expected 'one-at-a-time or 'all, got "
                      (object->string mode)
                    ) ;string-append
        ) ;type-error
      ) ;if
    ) ;define

    (define (make-pi-pending-queue)
      (%make-pi-pending-queue 'one-at-a-time (make-list-queue '()))
    ) ;define

    (define (pi-pending-mode q)
      (%pi-pending-queue-mode q)
    ) ;define

    (define (pi-pending-mode-set! q mode)
      (%pi-pending-queue-mode-set! q (%pi-pending-check-mode mode))
    ) ;define

    (define (pi-pending-empty? q)
      (list-queue-empty? (%pi-pending-queue-items q))
    ) ;define

    (define (pi-pending-length q)
      (length (list-queue-list (%pi-pending-queue-items q)))
    ) ;define

    (define (pi-pending-items q)
      (list-queue-list (%pi-pending-queue-items q))
    ) ;define

    ;; 入队：追加到队尾，返回当前长度
    (define (pi-pending-enqueue! q msg)
      (list-queue-add-back! (%pi-pending-queue-items q) msg)
      (pi-pending-length q)
    ) ;define

    ;; 预览下次投递将取走的消息，不移除：
    ;; one-at-a-time 只看队首一条，all 看全部
    (define (pi-pending-peek q)
      (if (eq? (%pi-pending-queue-mode q) 'all)
        (pi-pending-items q)
        (let ((items (pi-pending-items q)))
          (if (pair? items) (list (car items)) '())
        ) ;let
      ) ;if
    ) ;define

    ;; 消费：取走 peek 选中部分并从队列移除
    (define (pi-pending-drain! q)
      (let ((picked (pi-pending-peek q)))
        (if (null? picked)
          '()
          (let loop
            ((rest picked))
            (list-queue-remove-front! (%pi-pending-queue-items q))
            (if (pair? (cdr rest)) (loop (cdr rest)))
          ) ;let
        ) ;if
        picked
      ) ;let
    ) ;define

    ;; 清空并返回全部剩余消息（ESC 撤回回编辑器用）
    (define (pi-pending-clear! q)
      (list-queue-remove-all! (%pi-pending-queue-items q))
    ) ;define

  ) ;begin
) ;define-library
