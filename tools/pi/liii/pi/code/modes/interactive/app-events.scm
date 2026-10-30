;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 消息卡片、队列调度与后台事件处理
(define-library (liii pi code modes interactive app-events)
  (export pi-app-chat pi-app-add-user-message! pi-app-add-tool-result!
    pi-app-add-assistant-text! pi-app-add-retry-message! pi-app-drain-queue!
    pi-app-poll-events! %pi-app-job-payload pi-app-queue-steering!
    pi-app-queue-follow-up! pi-app-remove-pending-message!
    pi-app-restore-queues! pi-app-toggle-thinking!
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
    (liii pi code modes interactive bash)
    (liii pi code modes interactive app-name)
    (liii pi code modes interactive app-resume)
    (liii pi code modes interactive app-session)
    (liii pi code modes interactive common)
    (liii pi code modes interactive footer)
  ) ;import
  (begin

    ;; 以下 add-*! 系列仅做容器变更，统一由 pi-app-drain-queue! / pi-app-render! 上屏
    (define (pi-app-chat app)
      (container-children (%pi-app-chat app))
    ) ;define

    ;; 对话记录内容变化后，滚动视口的行数缓存失效：
    ;; 下一次统计行数时重新渲染内容（缓存只是滚动上界的换算依据）
    (define (%pi-app-invalidate-scroll! app)
      (scroll-view-invalidate! (pi-app-scroll-view app))
    ) ;define

    (define (pi-app-add-user-message! app text)
      (%pi-app-invalidate-scroll! app)
      (container-add! (%pi-app-chat app) (make-user-card text))
    ) ;define

    (define (pi-app-add-tool-result! app tool-name args status . maybe-result)
      (let ((res (if (pair? maybe-result) (car maybe-result) #f)))
        (%pi-app-invalidate-scroll! app)
        (container-add! (%pi-app-chat app) (make-tool-card tool-name args status res))
      ) ;let
    ) ;define

    (define (pi-app-add-assistant-text! app text)
      (%pi-app-invalidate-scroll! app)
      (container-add! (%pi-app-chat app) (make-assistant-card text))
    ) ;define

    (define (pi-app-add-retry-message! app msg)
      (%pi-app-invalidate-scroll! app)
      (container-add! (%pi-app-chat app) (make-retry-card msg))
    ) ;define

    (define (%pi-app-close-thinking-card! app)
      (%pi-app-thinking-card-set! app #f)
    ) ;define

    (define (%pi-app-handle-thinking-line! app line)
      (%pi-app-invalidate-scroll! app)
      (let ((card (pi-app-thinking-card app)))
        (if card
          (thinking-card-append-line! card line)
          ;; 新建卡片继承 Ctrl+T 当前隐藏状态（对齐上游 hideThinkingBlock 传入构造器）
          (let ((new-card (make-thinking-card line (pi-app-hide-thinking? app))))
            (%pi-app-thinking-card-set! app new-card)
            (container-add! (%pi-app-chat app) new-card)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; Ctrl+T 全局切换思考块显隐（对齐上游 toggleThinkingBlockVisibility）：
    ;; 翻转标志 → 已有卡片即时折叠/展开 → 状态行提示 → 持久化回调；
    ;; 新建卡片在 %pi-app-handle-thinking-line! 中继承新状态
    (define (pi-app-toggle-thinking! app)
      (let ((next (not (pi-app-hide-thinking? app))))
        (%pi-app-hide-thinking?-set! app next)
        (for-each (lambda (child)
                    (when (thinking-card-component? child)
                      (thinking-card-set-hidden! child next)
                    ) ;when
                  ) ;lambda
          (container-children (%pi-app-chat app))
        ) ;for-each
        (%pi-app-show-status! app
          (string-append "Thinking blocks: " (if next "hidden" "visible"))
        ) ;%pi-app-show-status!
        (let ((save (%pi-app-on-save-hide-thinking app)))
          (when save
            (save next)
          ) ;when
        ) ;let
        next
      ) ;let
    ) ;define

    ;; job 载荷：(消息 thinking-level provider model-id)，
    ;; 会话内 /model 与 /thinking 切换均对下一条消息生效（对齐上游 per-request 覆盖）
    (define (%pi-app-job-payload app msg)
      (list msg (pi-app-thinking-level app) (pi-app-provider app) (pi-app-model app))
    ) ;define

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
                  ;; 新任务开始：关闭上一轮引用，已显示的 thinking 内容保留
                  (%pi-app-close-thinking-card! app)
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
                    ;; 消息携带当前 thinking level 与 provider/model，会话内切换后下一条消息生效
                    (begin
                      (chan-send! jobs (%pi-app-job-payload app msg) 1000)
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

    (define (%pi-app-find-tool-card app id)
      (let ((entry (assoc id (%pi-app-tool-cards app))))
        (and entry (cdr entry))
      ) ;let
    ) ;define

    (define (%pi-app-handle-tool-start! app ev)
      (let* ((id (cadr ev))
             (name (caddr ev))
             (args (cadddr ev))
             (existing (%pi-app-find-tool-card app id))
            ) ;
        (unless existing
          (let ((card (make-tool-card name args 'pending)))
            (tool-card-set-expanded! card (pi-app-tools-expanded? app))
            (%pi-app-tool-cards-set! app (cons (cons id card) (%pi-app-tool-cards app)))
            (container-add! (%pi-app-chat app) card)
          ) ;let
        ) ;unless
      ) ;let*
    ) ;define

    (define (%pi-app-handle-tool-update! app ev)
      (let ((card (%pi-app-find-tool-card app (cadr ev))))
        (when card
          (tool-card-set-output! card (car (cddddr ev)))
        ) ;when
      ) ;let
    ) ;define

    (define (%pi-app-handle-tool-end! app ev)
      (if (>= (length ev) 6)
        ;; 新事件：(end id name args is-error? result [duration-ms])
        (let* ((id (cadr ev))
               (name (caddr ev))
               (args (cadddr ev))
               (is-err? (list-ref ev 4))
               (raw-res (list-ref ev 5))
               (duration (and (> (length ev) 6) (list-ref ev 6)))
               (card (%pi-app-find-tool-card app id))
              ) ;
          (if card
            (tool-card-set-complete! card raw-res is-err? duration)
            ;; 兼容未收到 start 的恢复/测试路径
            (let ((new-card (make-tool-card name args 'pending)))
              (tool-card-set-expanded! new-card (pi-app-tools-expanded? app))
              (tool-card-set-complete! new-card raw-res is-err? duration)
              (container-add! (%pi-app-chat app) new-card)
            ) ;let
          ) ;if
        ) ;let*
        ;; 旧自定义 handler：(end name args is-error? result)
        (let ((name (cadr ev))
              (args (caddr ev))
              (is-err? (cadddr ev))
              (raw-res (car (cddddr ev)))
             ) ;
          (pi-app-add-tool-result! app name args (if is-err? 'error 'success) raw-res)
        ) ;let
      ) ;if
    ) ;define

    (define (%pi-app-handle-event! app ev)
      (case (car ev)
            ((bash_complete) (%pi-app-handle-bash-complete! app ev))
            ((session_name_changed session_name_error) (pi-app-handle-name-event! app ev))
            ((session_stats session_stats_error) (pi-app-handle-session-event! app ev))
            ((session_resumed session_resume_error) (pi-app-handle-resume-event! app ev))
            ;; 状态条目落盘失败：仅以警告卡片上屏，不影响已生效的界面状态
            ((session_state_error)
             (pi-app-add-tool-result! app "session" '() 'error (cadr ev))
            ) ;
            ((branch_changed) (%pi-app-handle-branch-changed! app ev))
            ((tool_execution_start) (%pi-app-handle-tool-start! app ev))
            ((tool_execution_update) (%pi-app-handle-tool-update! app ev))
            ((tool_execution_end) (%pi-app-handle-tool-end! app ev))
            ((assistant_thinking_line) (%pi-app-handle-thinking-line! app (cadr ev)))
            ((assistant_thinking_end) (%pi-app-close-thinking-card! app))
            ((assistant_text) (pi-app-add-assistant-text! app (cadr ev)))
            ((retry) (pi-app-add-retry-message! app (cadr ev)))
            ;; worker 消费了 steering / follow-up 注入消息：
            ;; 移除待处理显示并上屏正式用户卡片（对齐上游 message_start 双动作）
            ((user_message)
             (pi-app-remove-pending-message! app (cadr ev))
             (pi-app-add-user-message! app (cadr ev))
            ) ;
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
                     (%pi-app-close-thinking-card! app)
                     (when (and (> (length ev) 1) (cadr ev))
                       (pi-app-add-assistant-text! app "Aborted")
                     ) ;when
                     (pi-queue-finish-task! q)
                     (settle!)
                    ) ;
                    ((error)
                     (%pi-app-close-thinking-card! app)
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
