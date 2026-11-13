;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 队列注入与撤回：steering / follow-up 消息的通道写入、显示列表同步，
;; 以及 ESC 撤回时的队列还原，从 app-events.scm 拆出以控制单文件行数
(define-library (liii pi code modes interactive app-queue)
  (export pi-app-queue-steering! pi-app-queue-follow-up!
    pi-app-remove-pending-message! pi-app-restore-queues!
    %pi-app-send-abort-signals! %pi-app-restore-to-editor! pi-app-handle-escape!
  ) ;export
  (import (scheme base)
    (liii go)
    (liii list)
    (liii string)
    (liii tui)
    (liii pi agent queue)
    (liii pi code modes interactive state)
    (liii pi code modes interactive render)
    (liii pi code modes interactive app-bash)
  ) ;import
  (begin

    ;; 安全带超时的通道发送：通道异常（已关闭等）时静默降级，仅保留显示列表
    (define (%chan-offer! ch msg)
      (catch #t (lambda () (chan-send! ch msg 1000)) (lambda args #f))
    ) ;define

    ;; 列表移除首个等于 text 的元素（对齐上游 indexOf+splice 的消费同步）
    (define (%remove-first texts text)
      (let loop
        ((rest texts) (acc '()))
        (cond ((null? rest) (reverse acc))
              ((equal? (car rest) text) (append (reverse acc) (cdr rest)))
              (else (loop (cdr rest) (cons (car rest) acc)))
        ) ;cond
      ) ;let
    ) ;define

    ;; steering 注入（busy 时回车）：写入通道 + 追加显示列表
    (define (pi-app-queue-steering! app text)
      (%chan-offer! (pi-app-steering-chan app) text)
      (%pi-app-steering-pending-set! app
        (append (%pi-app-steering-pending app) (list text))
      ) ;%pi-app-steering-pending-set!
    ) ;define

    ;; follow-up 注入（busy 时 alt+enter）：写入通道 + 追加显示列表
    (define (pi-app-queue-follow-up! app text)
      (%chan-offer! (pi-app-follow-up-chan app) text)
      (%pi-app-follow-up-pending-set! app
        (append (%pi-app-follow-up-pending app) (list text))
      ) ;%pi-app-follow-up-pending-set!
    ) ;define

    ;; worker 消费注入消息：优先从 steering 显示移除，其次 follow-up
    ;; （与 worker 侧 steering 优先的消费顺序一致）
    (define (pi-app-remove-pending-message! app text)
      (let ((steering (%pi-app-steering-pending app)))
        (if (member text steering)
          (%pi-app-steering-pending-set! app (%remove-first steering text))
          (%pi-app-follow-up-pending-set! app
            (%remove-first (%pi-app-follow-up-pending app) text)
          ) ;%pi-app-follow-up-pending-set!
        ) ;if
      ) ;let
    ) ;define

    ;; 非阻塞排空通道（ESC 撤回时与显示列表对齐后丢弃残余）
    (define (%drain-chan ch)
      (let loop
        ((acc '()))
        (let ((m (catch #t (lambda () (chan-try-recv! ch #f)) (lambda args #f))))
          (if (and (string? m) (not (string-null? m))) (loop (cons m acc)) (reverse acc))
        ) ;let
      ) ;let
    ) ;define

    ;; 通道残余中不在显示列表里的部分（防御性兜底，正常恒为空）
    (define (%chan-leftovers drained shown)
      (cond ((null? drained) '())
            ((member (car drained) shown) (%chan-leftovers (cdr drained) shown))
            (else (cons (car drained) (%chan-leftovers (cdr drained) shown)))
      ) ;cond
    ) ;define

    ;; 撤回全部排队消息（ESC）：steering → follow-up → 等待队列，
    ;; 按上游顺序合并返回；同时排空两个通道并清空显示列表
    (define (pi-app-restore-queues! app)
      (let* ((steering (%pi-app-steering-pending app))
             (follow-up (%pi-app-follow-up-pending app))
             (items (pi-queue-restore-all! (pi-app-queue app)))
             (chan-steer (%drain-chan (pi-app-steering-chan app)))
             (chan-fu (%drain-chan (pi-app-follow-up-chan app)))
             (leftovers (%chan-leftovers (append chan-steer chan-fu) (append steering follow-up))
             ) ;leftovers
             (all (append steering follow-up items leftovers))
            ) ;
        (%pi-app-steering-pending-set! app '())
        (%pi-app-follow-up-pending-set! app '())
        all
      ) ;let*
    ) ;define

    ;; ESC 与退出共用的中止信号发射：agent 任务（busy 时）与运行中的 ! 命令；
    ;; bash 通道仅在命令运行时发送，避免陈旧信号误伤下一个命令
    (define (%pi-app-send-abort-signals! app q)
      (when (pi-queue-busy? q)
        (catch #t
          (lambda () (chan-send! (pi-app-abort-chan app) 'abort 0))
          (lambda args #f)
        ) ;catch
      ) ;when
      (when (pi-app-bash-running? app)
        (catch #t
          (lambda () (chan-send! (pi-app-bash-abort-chan app) 'abort 0))
          (lambda args #f)
        ) ;catch
      ) ;when
    ) ;define

    ;; 撤回排队消息到编辑器：排队内容在前、编辑器当前内容在后，\n\n 连接
    (define (%pi-app-restore-to-editor! app)
      (let* ((ed (pi-app-editor app))
             (restored (pi-app-restore-queues! app))
             (combined
               (string-join
                 (filter
                   (lambda (s) (and (string? s) (not (string-null? s))))
                   (list (string-join restored "\n\n") (editor-text ed))
                 ) ;filter
                 "\n\n"
               ) ;string-join
             ) ;combined
            ) ;
        (unless (string-null? combined)
          (editor-set-text! ed combined)
        ) ;unless
      ) ;let*
    ) ;define

    ;; ESC 键处理：清除选区或中止在途任务并撤回排队消息
    ;; 返回 #t
    (define (pi-app-handle-escape! app)
      (let ((q (pi-app-queue app)))
        (if (pi-app-selection-active? app)
          (begin
            (pi-app-selection-clear! app)
            (pi-app-render! app)
            #t
          ) ;begin
          (begin
            (%pi-app-send-abort-signals! app q)
            (when (pi-queue-busy? q)
              (pi-queue-set-cancelling! q)
            ) ;when
            (%pi-app-restore-to-editor! app)
            (pi-app-render! app)
            #t
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
