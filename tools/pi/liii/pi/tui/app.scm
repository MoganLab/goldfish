;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi tui app)
  (export make-pi-app pi-app? pi-app-term pi-app-screen pi-app-editor
    pi-app-queue pi-app-chat pi-app-render! pi-app-submit! pi-app-drain-queue!
    pi-app-add-user-message! pi-app-add-tool-result! pi-app-add-assistant-text!
    pi-app-add-retry-message! pi-app-jobs-chan pi-app-event-chan
    pi-app-poll-events!
  ) ;export
  (import (scheme base)
    (liii go)
    (liii time)
    (liii string)
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
      (term pi-app-term)
      (screen pi-app-screen)
      (doc %pi-app-doc)
      (chat %pi-app-chat)
      (pending-box %pi-app-pending-box)
      (editor pi-app-editor %pi-app-editor-set!)
      (footer %pi-app-footer)
      (queue pi-app-queue)
      (handler %pi-app-handler)
      (jobs-chan pi-app-jobs-chan)
      (event-chan pi-app-event-chan)
    ) ;define-record-type

    (define %pi-app-term pi-app-term)

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
        (let* ((raw-doc-lines (component-render (%pi-app-doc app) w))
               ;; 规范化拆分可能包含 \n 的字符串行，确保 ed-y 计算与 screen-render! 完全对齐
               (doc-lines
                 (let loop
                   ((rest raw-doc-lines) (acc '()))
                   (if (null? rest)
                     (reverse acc)
                     (let ((item (car rest)))
                       (if (string-contains? item "\n")
                         (loop (cdr rest) (append (reverse (string-split item "\n")) acc))
                         (loop (cdr rest) (cons item acc))
                       ) ;if
                     ) ;let
                   ) ;if
                 ) ;let
               ) ;doc-lines
               (ed (pi-app-editor app))
               (ed-lines (component-render ed w))
               (footer-lines (component-render (%pi-app-footer app) w))
               (ed-cur (editor-cursor ed))
               (ed-y (+ (length doc-lines) (if %pi-editor-top-border 1 0)))
               (cur-pos (cons (editor-display-col ed) (+ ed-y (car ed-cur))))
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
                    ((tool_execution_end)
                     (let ((name (cadr ev))
                           (args (caddr ev))
                           (is-err? (cadddr ev))
                           (raw-res (car (cddddr ev)))
                          ) ;
                       (pi-app-add-tool-result! app name args (if is-err? 'error 'success) raw-res)
                     ) ;let
                    ) ;
                    ((assistant_text) (pi-app-add-assistant-text! app (cadr ev)))
                    ((retry) (pi-app-add-retry-message! app (cadr ev)))
                    ((agent_settled) (pi-queue-finish-task! q) (settle!))
                    ((error)
                     (pi-app-add-tool-result! app "error" '() 'error (cadr ev))
                     (pi-queue-finish-task! q)
                     (settle!)
                    ) ;
                    (else #t)
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

    ;; 队列调度核心推进逻辑：
    ;; 消费前先上屏深蓝卡片并即时渲染刷新，再派发任务，保证按回车瞬间响应不卡顿
    ;; 返回 #t 表示本次消费并渲染了消息卡片
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
                    (begin
                      (chan-send! jobs msg 1000)
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

    ;; 用户提交动作：消息统一先进入等待队列并清空输入框；
    ;; 队列忙时立即渲染出 [Queued] 提示，闲时由 drain-queue! 渲染唯一一帧
    (define (pi-app-submit! app text)
      (let ((clean (string-trim text)))
        (unless (string-null? clean)
          (let ((q (pi-app-queue app)) (ed (pi-app-editor app)))
            (pi-queue-enqueue! q clean)
            (editor-set-text! ed "")
            (when (pi-queue-busy? q)
              (pi-app-render! app)
            ) ;when
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
             (branch (detect-git-branch))
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

  ) ;begin
) ;define-library
