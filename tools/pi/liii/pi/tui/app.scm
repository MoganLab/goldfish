;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi tui app)
  (export make-pi-app pi-app? pi-app-screen pi-app-editor pi-app-queue
    pi-app-chat pi-app-render! pi-app-run pi-app-submit! pi-app-drain-queue!
    pi-app-add-user-message! pi-app-add-tool-result! pi-app-add-assistant-text!
    pi-app-add-retry-message! pi-app-jobs-chan pi-app-event-chan pi-app-poll-events!
  ) ;export
  (import (scheme base)
    (liii go)
    (liii time)
    (liii either)
    (liii string)
    (liii subprocess)
    (liii tui)
    (liii pi agent queue)
    (liii pi tui views)
    (liii pi tui worker)
    (liii pi tui common)
    (liii pi tui header)
    (liii pi tui footer)
  ) ;import
  (begin

    ;; 编辑器顶边框：开启时占 1 行，渲染时光标行需跳过该行
    (define %pi-editor-top-border #t)

    (define-record-type <pi-app>
      (%make-pi-app term screen doc chat pending-box editor footer queue handler
        jobs-chan event-chan
      ) ;%make-pi-app
      pi-app?
      (term %pi-app-term)
      (screen pi-app-screen)
      (doc %pi-app-doc)
      (chat %pi-app-chat)
      (pending-box %pi-app-pending-box)
      (editor pi-app-editor %pi-app-editor-set!)
      (footer %pi-app-footer)
      (queue pi-app-queue)
      (handler %pi-app-handler)
      (jobs-chan %pi-app-jobs-chan)
      (event-chan %pi-app-event-chan)
    ) ;define-record-type

    (define (%detect-git-branch)
      (catch #t
        (lambda ()
          (let ((res (run-either '("git" "branch" "--show-current")
                       :stdout  'capture
                       :stderr  'discard
                       :timeout 2
                     ) ;run-either
                ) ;res
               ) ;
            (if (either-right? res)
              (let ((b (string-trim-both (to-right res))))
                (if (string-null? b) #f b)
              ) ;let
              #f
            ) ;if
          ) ;let
        ) ;lambda
        (lambda args #f)
      ) ;catch
    ) ;define

    (define (pi-app-render! app)
      (let* ((screen (pi-app-screen app))
             (term (%pi-app-term app))
             (w (car (terminal-size term)))
             ;; pending 容器内容纯由队列状态派生：busy 时展示队首等待消息
             (q (pi-app-queue app))
             (pending-box (%pi-app-pending-box app))
            ) ;
        (container-clear! pending-box)
        (when (pi-queue-busy? q)
          (let ((items (pi-queue-items q)))
            (unless (null? items)
              (container-add! pending-box (make-queued-card (car items)))
            ) ;unless
          ) ;let
        ) ;when
        (let* ((doc-lines (component-render (%pi-app-doc app) w))
               (ed (pi-app-editor app))
               (ed-lines (component-render ed w))
               (footer-lines (component-render (%pi-app-footer app) w))
               (ed-cur (editor-cursor ed))
               (ed-y (+ (length doc-lines) (if %pi-editor-top-border 1 0)))
               (cur-pos (cons (cadr ed-cur) (+ ed-y (car ed-cur))))
              ) ;
          (screen-render! screen (append doc-lines ed-lines footer-lines) cur-pos)
        ) ;let*
      ) ;let*
    ) ;define

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

    (define (pi-app-jobs-chan app)
      (%pi-app-jobs-chan app)
    ) ;define

    (define (pi-app-event-chan app)
      (%pi-app-event-chan app)
    ) ;define

    ;; 非阻塞轮询来自后台 Worker 的事件流，驱动卡片实时上屏与队列结算
    (define (pi-app-poll-events! app)
      (let ((ch (%pi-app-event-chan app)) (q (pi-app-queue app)) (has-event #f))
        (let loop
          ()
          (let ((ev (chan-try-recv! ch #f)))
            (when ev
              (set! has-event #t)
              (let ((type (car ev)))
                (cond
                 ((eq? type 'tool_execution_end)
                  (let ((name (cadr ev))
                        (args (caddr ev))
                        (is-err? (cadddr ev))
                        (raw-res (car (cddddr ev)))
                       ) ;
                    (pi-app-add-tool-result! app name args (if is-err? 'error 'success) raw-res)
                  ) ;let
                 ) ;
                 ((eq? type 'assistant_text) (pi-app-add-assistant-text! app (cadr ev)))
                 ((eq? type 'retry) (pi-app-add-retry-message! app (cadr ev)))
                 ((eq? type 'agent_settled) (pi-queue-finish-task! q) (pi-app-drain-queue! app))
                 ((eq? type 'error)
                  (pi-app-add-tool-result! app "error" '() 'error (cadr ev))
                  (pi-queue-finish-task! q)
                  (pi-app-drain-queue! app)
                 ) ;
                ) ;cond
              ) ;let
              (loop)
            ) ;when
          ) ;let
        ) ;let
        (when has-event
          (pi-app-render! app)
        ) ;when
      ) ;let
    ) ;define

    ;; 队列调度核心推进逻辑：
    ;; 消费前先上屏深蓝卡片并即时渲染刷新，再执行 handler，保证按回车瞬间响应不卡顿
    (define (pi-app-drain-queue! app)
      (let ((q (pi-app-queue app))
            (jobs (%pi-app-jobs-chan app))
            (handler (%pi-app-handler app))
           ) ;
        (unless (pi-queue-busy? q)
          (let loop
            ()
            (let ((msg (pi-queue-dequeue-next! q)))
              (when msg
                ;; 1. 出队并正式上屏为深蓝用户消息卡片
                (pi-app-add-user-message! app msg)
                ;; 2. 关键：在进入后台处理前立即渲染上屏，用户按回车瞬间看到消息已发送
                (pi-app-render! app)
                ;; 3. 任务派发：
                (if handler
                  ;; 自定义 handler 分支（单元测试或特定扩展）：
                  (let ((ret (handler app msg)))
                    (unless (eq? ret 'async)
                      (pi-queue-finish-task! q)
                      (pi-app-render! app)
                      (loop)
                    ) ;unless
                  ) ;let
                  ;; 默认真实分支：通过 (liii go) 后台 worker 并发执行，主线程立即返回继续响应按键！
                  (chan-send! jobs msg 1000)
                ) ;if
              ) ;when
            ) ;let
          ) ;let
        ) ;unless
      ) ;let
    ) ;define

    ;; 用户提交动作：消息统一先进入等待队列，清空输入框并立即刷新渲染
    (define (pi-app-submit! app text)
      (let ((clean (string-trim text)))
        (unless (string-null? clean)
          (let ((q (pi-app-queue app)) (ed (pi-app-editor app)))
            (pi-queue-enqueue! q clean)
            (editor-set-text! ed "")
            (pi-app-render! app)
            (pi-app-drain-queue! app)
          ) ;let
        ) ;unless
      ) ;let
    ) ;define

    (define (make-pi-app . args)
      (let* ((term (%keyword-value :terminal args #f))
             (handler-opt (%keyword-value :handler args #f))
             (actual-term (or term (make-standard-terminal :inline #t)))
             (screen (make-main-screen actual-term :auto-hide-cursor #f))
             (header (make-pi-header))
             (chat (make-container))
             (pending-box (make-container))
             (doc (make-container))
             (branch (%detect-git-branch))
             (footer (make-pi-footer :branch branch))
             (queue (make-pi-message-queue))
             (jobs-chan (make-chan 16))
             (event-chan (make-chan 32))
             (app (%make-pi-app actual-term screen doc chat pending-box #f
                    footer queue handler-opt jobs-chan event-chan
                  ) ;%make-pi-app
             ) ;app
             (ed (make-editor ""
                   :multiline     #f
                   :top-border    %pi-editor-top-border
                   :bottom-border #t
                   :border-color  "\x1b;[38;2;129;162;190m"
                   :focused       #t
                   :cursor-style  'hardware
                   :on-submit     (lambda (self txt) (pi-app-submit! app txt))
                 ) ;make-editor
             ) ;ed
            ) ;
        (container-add! doc header)
        (container-add! doc chat)
        (container-add! doc pending-box)
        (%pi-app-editor-set! app ed)
        ;; 若未指定自定义 handler，启动后台 (liii go) 独立工作协程
        (unless handler-opt
          (go (pi-tui-agent-worker jobs-chan event-chan))
        ) ;unless
        app
      ) ;let*
    ) ;define

    (define (pi-app-run app)
      (let* ((term (%pi-app-term app))
             (screen (pi-app-screen app))
             (ed (pi-app-editor app))
             (q (pi-app-queue app))
            ) ;
        (dynamic-wind (lambda ()
                        (terminal-enter-raw-mode! term)
                        (terminal-hide-cursor! term)
                        (pi-app-render! app)
                      ) ;lambda
          (lambda ()
            (let loop
              ()
              ;; 优先轮询来自后台 Worker 的事件流上屏
              (pi-app-poll-events! app)
              (let ((k (terminal-poll-key term)))
                (cond ((or (eq? k 'ctrl-c) (eq? k 'ctrl-d)) #t)
                      ;; 按 Escape 时：若队列有等待中的消息，撤回至 editor 重新编辑
                      ((eq? k 'escape)
                       (let ((restored (pi-queue-restore-all! q)))
                         (if (pair? restored)
                           (begin
                             (editor-set-text! ed (string-join restored "\n"))
                             (pi-app-render! app)
                             (loop)
                           ) ;begin
                           #t
                         ) ;if
                       ) ;let
                      ) ;
                      ((not k) (sleep 0.01) (loop))
                      (else (component-handle-input ed k) (pi-app-render! app) (loop))
                ) ;cond
              ) ;let
            ) ;let
          ) ;lambda
          (lambda ()
            (let* ((lines (screen-lines screen)) (total (length lines)))
              (when (> total 0)
                (terminal-set-cursor! term 0 total)
              ) ;when
              (terminal-show-cursor! term)
              (terminal-exit-raw-mode! term)
              (terminal-write term "\r\n")
              (terminal-flush term)
            ) ;let*
          ) ;lambda
        ) ;dynamic-wind
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
