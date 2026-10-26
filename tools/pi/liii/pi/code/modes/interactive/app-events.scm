;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 消息卡片、队列调度与后台事件处理
(define-library (liii pi code modes interactive app-events)
  (export pi-app-chat pi-app-add-user-message! pi-app-add-tool-result!
    pi-app-add-assistant-text! pi-app-add-retry-message! pi-app-drain-queue!
    pi-app-poll-events!
  ) ;export
  (import (scheme base)
    (liii go)
    (liii tui)
    (liii pi agent queue)
    (liii pi code modes interactive state)
    (liii pi code modes interactive render)
    (liii pi code modes interactive views)
    (liii pi code modes interactive working)
    (liii pi code modes interactive app-bash)
    (liii pi code modes interactive app-name)
    (liii pi code modes interactive app-session)
    (liii pi code modes interactive common)
    (liii pi code modes interactive footer)
  ) ;import
  (begin

    ;; 以下 add-*! 系列仅做容器变更，统一由 pi-app-drain-queue! / pi-app-render! 上屏
    (define (pi-app-chat app)
      (container-children (%pi-app-chat app))
    ) ;define

    (define (pi-app-add-user-message! app text)
      (container-add! (%pi-app-chat app) (make-user-card text))
    ) ;define

    (define (pi-app-add-tool-result! app tool-name args status . maybe-result)
      (let ((res (if (pair? maybe-result) (car maybe-result) #f)))
        (container-add! (%pi-app-chat app) (make-tool-card tool-name args status res))
      ) ;let
    ) ;define

    (define (pi-app-add-assistant-text! app text)
      (container-add! (%pi-app-chat app) (make-assistant-card text))
    ) ;define

    (define (pi-app-add-retry-message! app msg)
      (container-add! (%pi-app-chat app) (make-retry-card msg))
    ) ;define

    ;; 队列调度：消费前先上屏深蓝卡片并即时渲染；返回 #t 表示本次消费并渲染
    (define (pi-app-drain-queue! app)
      (let ((q (pi-app-queue app))
            (jobs (pi-app-jobs-chan app))
            (handler (%pi-app-handler app))
           ) ;
        (if (pi-queue-busy? q)
          #f
          (let loop
            ((rendered #f))
            (let ((msg (pi-queue-dequeue-next! q)))
              (if (not msg)
                rendered
                (begin
                  ;; 1. 出队并正式上屏为深蓝用户消息卡片
                  (pi-app-add-user-message! app msg)
                  ;; 2. 关键：在进入后台处理前立即渲染上屏，用户按回车瞬间看到消息已发送
                  ;;    同时复位 Working 指示器，保证任务期间从 ⠋ 帧起动画
                  (working-reset! (pi-app-working app))
                  (pi-app-render! app)
                  ;; 3. 任务派发：
                  (if handler
                    ;; 自定义 handler 分支（单元测试或特定扩展）：同步执行并结算
                    (begin
                      (handler app msg)
                      (pi-queue-finish-task! q)
                      (pi-app-render! app)
                      (loop #t)
                    ) ;begin
                    ;; 默认真实分支：通过 (liii go) 后台 worker 并发执行，主线程立即返回继续响应按键！
                    ;; 消息携带当前 thinking level，会话内切换后下一条消息生效
                    (begin
                      (chan-send! jobs (list msg (pi-app-thinking-level app)) 1000)
                      #t
                    ) ;begin
                  ) ;if
                ) ;begin
              ) ;if
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%pi-app-handle-bash-complete! app ev)
      ;; 后台 bash 执行完成：按 id 回填卡片输出与退出码；
      ;; 未排除的结果（! 前缀）经 record-sink 注入会话上下文
      (let* ((id (cadr ev))
             (command (caddr ev))
             (text (cadddr ev))
             (code (list-ref ev 4))
             (excluded? (and (> (length ev) 5) (list-ref ev 5)))
            ) ;
        (let ((entry (assq id (%pi-app-bash-cards app))))
          (when entry
            (bash-card-set-output! (cdr entry) text)
            (bash-card-set-complete! (cdr entry) code)
          ) ;when
        ) ;let
        (%pi-app-record-bash! app command text code excluded?)
      ) ;let*
    ) ;define

    (define (%pi-app-handle-branch-changed! app ev)
      ;; (liii fs-watch) 检测到 .git/HEAD 变化：携带 git-dir 时直接读
      ;; HEAD 文件解析（避免 CI 子进程并发退出竞态）；否则按 cwd 探测
      (let* ((git-dir (if (> (length ev) 1) (cadr ev) #f))
             (branch (if (string? git-dir) (git-branch-from-head-file git-dir) (detect-git-branch))
             ) ;branch
            ) ;
        (unless (equal? branch (pi-footer-branch (%pi-app-footer app)))
          (pi-footer-set-branch! (%pi-app-footer app) branch)
        ) ;unless
      ) ;let*
    ) ;define

    (define (%pi-app-handle-tool-end! app ev)
      (let ((name (cadr ev))
            (args (caddr ev))
            (is-err? (cadddr ev))
            (raw-res (car (cddddr ev)))
           ) ;
        (pi-app-add-tool-result! app name args (if is-err? 'error 'success) raw-res)
      ) ;let
    ) ;define

    (define (%pi-app-handle-event! app ev)
      (case (car ev)
            ((bash_complete) (%pi-app-handle-bash-complete! app ev))
            ((session_name_changed session_name_error) (pi-app-handle-name-event! app ev))
            ((session_stats session_stats_error) (pi-app-handle-session-event! app ev))
            ((branch_changed) (%pi-app-handle-branch-changed! app ev))
            ((tool_execution_end) (%pi-app-handle-tool-end! app ev))
            ((assistant_text) (pi-app-add-assistant-text! app (cadr ev)))
            ((retry) (pi-app-add-retry-message! app (cadr ev)))
            (else #t)
      ) ;case
    ) ;define

    ;; 非阻塞轮询来自后台 Worker 的事件流，驱动卡片实时上屏与队列结算
    (define (pi-app-poll-events! app)
      (let ((ch (pi-app-event-chan app)) (q (pi-app-queue app)) (has-event #f))
        ;; 结算队列并接续下一条消息；drain 已渲染时复位 has-event，避免双重渲染
        (define (settle!)
          (when (pi-app-drain-queue! app)
            (set! has-event #f)
          ) ;when
        ) ;define
        (let loop
          ()
          (let ((ev (chan-try-recv! ch #f)))
            (when ev
              (set! has-event #t)
              (case (car ev)
                    ;; 任务结算：aborted 时先上屏 Aborted 提示卡片（对齐上游
                    ;; pi 的 Aborted 状态显示），再解除 busy 锁定
                    ((agent_settled)
                     (when (and (> (length ev) 1) (cadr ev))
                       (pi-app-add-assistant-text! app "Aborted")
                     ) ;when
                     (pi-queue-finish-task! q)
                     (settle!)
                    ) ;
                    ((error)
                     (pi-app-add-tool-result! app "error" '() 'error (cadr ev))
                     (pi-queue-finish-task! q)
                     (settle!)
                    ) ;
                    (else (%pi-app-handle-event! app ev))
              ) ;case
              (loop)
            ) ;when
          ) ;let
        ) ;let
        (when has-event
          (pi-app-render! app)
        ) ;when
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
