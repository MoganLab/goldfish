;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code tui app)
  (export make-pi-app pi-app? pi-app-term pi-app-screen pi-app-editor
    pi-app-queue pi-app-chat pi-app-render! pi-app-submit! pi-app-drain-queue!
    pi-app-add-user-message! pi-app-add-tool-result! pi-app-add-assistant-text!
    pi-app-add-retry-message! pi-app-jobs-chan pi-app-event-chan
    pi-app-abort-chan pi-app-poll-events! pi-app-working pi-app-working-tick!
    pi-app-thinking-level pi-app-supports-thinking? pi-app-cycle-thinking!
    pi-app-bash-mode? pi-app-tools-expanded? pi-app-toggle-tool-output!
    pi-app-bash-jobs-chan pi-app-run-bash!
  ) ;export
  (import (scheme base)
    (liii base)
    (liii go)
    (liii time)
    (liii date-time)
    (liii string)
    (liii list)
    (liii tui)
    (liii pi agent queue)
    (liii pi code tui state)
    (liii pi code tui render)
    (liii pi code tui views)
    (liii pi code tui thinking)
    (liii pi code tui working)
    (liii pi code tui worker)
    (liii pi code tui bash)
    (liii pi code tui app-bash)
    (liii pi code tui common)
    (liii pi code tui header)
    (liii pi code tui footer)
  ) ;import
  (begin

    ;; 以下 add-*! 系列仅做容器变更，统一由 pi-app-drain-queue! / pi-app-render! 上屏
    (define (pi-app-chat app)
      (container-children (%pi-app-chat app))
    ) ;define

    ;; 判断字符串是否以指定字符开头
    (define (%starts-with-char? s c)
      (and (> (string-length s) 0) (char=? (string-ref s 0) c))
    ) ;define

    ;; bash 模式边框联动（对齐 pi onChange + updateEditorBorderColor）：
    ;; 文本（去前导空白后）以 ! 开头 → 绿色；否则恢复当前 thinking 级别颜色
    (define (%pi-app-editor-on-change app ed text)
      (let ((bash? (%starts-with-char? (string-trim-left text) #\!)))
        (%pi-app-bash-mode?-set! app bash?)
        (editor-set-border-color! ed
          (if bash?
            pi-bash-border-color
            (pi-thinking-border-color (pi-app-thinking-level app))
          ) ;if
        ) ;editor-set-border-color!
      ) ;let
    ) ;define

    ;; bash 编排（! 执行、!! 排除、结果注入上下文、Ctrl+O 展开/折叠）已拆至
    ;; (liii pi code tui app-bash)，此处经 import 再导出保持对外接口不变

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
                    ((bash_complete)
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
                    ) ;
                    ((branch_changed)
                     ;; (liii fs-watch) 检测到 .git/HEAD 变化：携带 git-dir 时直接读
                     ;; HEAD 文件解析（避免 CI 子进程并发退出竞态）；否则按 cwd 探测
                     (let ((git-dir (if (> (length ev) 1) (cadr ev) #f)))
                       (let ((b (if (string? git-dir) (git-branch-from-head-file git-dir) (detect-git-branch))
                             ) ;b
                            ) ;
                         (unless (equal? b (pi-footer-branch (%pi-app-footer app)))
                           (pi-footer-set-branch! (%pi-app-footer app) b)
                         ) ;unless
                       ) ;let
                     ) ;let
                    ) ;
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

    ;; Working 动画节拍推进：
    ;; idle 时复位指示器并返回 #f；busy 时首拍立即渲染，
    ;; 之后每隔 80ms 推进一帧并重绘，返回 #t 表示本拍发生了渲染
    (define (pi-app-working-tick! app)
      (let ((q (pi-app-queue app)) (ind (pi-app-working app)))
        (if (not (pi-queue-busy? q))
          (begin
            (working-reset! ind)
            #f
          ) ;begin
          (let ((now (current-monotonic-millis)))
            (cond ((not (working-last-tick ind))
                   ;; busy 首拍：无论从何种路径进入 busy，都从 ⠋ 起始
                   (working-reset! ind)
                   (working-last-tick-set! ind now)
                   (pi-app-render! app)
                   #t
                  ) ;
                  ((>= (- now (working-last-tick ind)) %working-frame-interval-ms)
                   (working-advance! ind)
                   (working-last-tick-set! ind now)
                   (pi-app-render! app)
                   #t
                  ) ;
                  (else #f)
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
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

    ;; 用户提交动作：! 前缀拦截为 bash 模式（!cmd 结果注入上下文，
    ;; !!cmd 仅显示不注入）；其余消息统一先进入等待队列并清空输入框；
    ;; 队列忙时立即渲染出 [Queued] 提示，闲时由 drain-queue! 渲染唯一一帧
    (define (pi-app-submit! app text)
      (let ((clean (string-trim text)))
        (unless (string-null? clean)
          ;; bash 模式：!cmd 直接经 bash 执行（对齐 pi 的 !/!! 前缀行为）；
          ;; 命令为空（仅 "!" 或 "!!"）时回退为普通消息提交（对齐 pi 的 fall-through）
          (let* ((bang? (%starts-with-char? clean #\!))
                 (excluded? (and bang? (> (string-length clean) 1) (char=? (string-ref clean 1) #\!))
                 ) ;excluded?
                 (cmd-start (if excluded? 2 1))
                 (command
                   (and bang?
                     (not (string-null? (string-trim (substring clean cmd-start))))
                     (string-trim (substring clean cmd-start))
                   ) ;and
                 ) ;command
                ) ;
            (if command
              (pi-app-run-bash! app command :excluded? excluded?)
              (let ((q (pi-app-queue app)) (ed (pi-app-editor app)))
                (pi-queue-enqueue! q clean)
                (editor-set-text! ed "")
                (when (pi-queue-busy? q)
                  (pi-app-render! app)
                ) ;when
                (pi-app-drain-queue! app)
              ) ;let
            ) ;if
          ) ;let*
        ) ;unless
      ) ;let
    ) ;define

    ;; shift+tab 会话内循环切换 thinking level（对齐上游 pi 的 app.thinking.cycle）：
    ;; 同步 footer 右下角标签与编辑器边框/Working spinner 颜色；
    ;; 模型不支持 reasoning 时返回 #f 且状态不变
    (define (pi-app-cycle-thinking! app)
      (if (pi-app-supports-thinking? app)
        (let ((next (pi-next-thinking-level (pi-app-thinking-level app))))
          (%pi-app-thinking-level-set! app next)
          (pi-footer-set-thinking-level! (%pi-app-footer app) next)
          (editor-set-border-color! (pi-app-editor app) (pi-thinking-border-color next))
          next
        ) ;let
        #f
      ) ;if
    ) ;define

    ;; 参数名与调用侧关键字一一对应（:terminal/:handler/:bash-handler/:record-sink 等）
    (define* (make-pi-app (terminal #f)
               (handler #f)
               (bash-handler #f)
               (record-sink #f)
               (provider #f)
               (model #f)
               (thinking-level #f)
               (supports-thinking #t)
             ) ;make-pi-app
      (let* ((actual-term (or terminal (make-standard-terminal :inline #t)))
             (screen (make-main-screen actual-term :auto-hide-cursor #f))
             (header (make-pi-header))
             (chat (make-container))
             (pending-box (make-container))
             (doc (make-container))
             (branch (detect-git-branch))
             (tl (or thinking-level "off"))
             (footer (make-pi-footer
                       :branch            branch
                       :provider          provider
                       :model             model
                       :thinking-level    tl
                       :supports-thinking supports-thinking
                     ) ;make-pi-footer
             ) ;footer
             (queue (make-pi-message-queue))
             (jobs-chan (make-chan 16))
             (event-chan (make-chan 32))
             (abort-chan (make-chan 1))
             (bash-jobs-chan (make-chan 16))
             ;; ! 命令结果注入上下文的默认回调：投递 bash_record 任务到
             ;; jobs-chan（后台 worker 写入会话文件；agent 忙时在通道排队延迟注入）
             (actual-record-sink
               (or record-sink
                 (lambda (command text code)
                   (catch #t
                     (lambda () (chan-send! jobs-chan (list 'bash_record command text code) 1000))
                     (lambda args #f)
                   ) ;catch
                 ) ;lambda
               ) ;or
             ) ;actual-record-sink
             (app (%make-pi-app actual-term
                    screen
                    doc
                    chat
                    pending-box
                    #f
                    footer
                    queue
                    handler
                    jobs-chan
                    event-chan
                    abort-chan
                    (make-working-indicator)
                    tl
                    supports-thinking
                    #f
                    #f
                    '()
                    bash-jobs-chan
                    bash-handler
                    actual-record-sink
                  ) ;%make-pi-app
             ) ;app
             (ed (make-editor ""
                   :multiline     #f
                   :top-border    %pi-editor-top-border
                   :bottom-border #t
                   :border-color  (pi-thinking-border-color tl)
                   :focused       #t
                   :cursor-style  'hardware
                   :on-submit     (lambda (self txt) (pi-app-submit! app txt))
                   :on-change
                   (lambda (ed* txt) (%pi-app-editor-on-change app ed* txt))
                 ) ;make-editor
             ) ;ed
            ) ;
        (container-add! doc header)
        (container-add! doc chat)
        (container-add! doc pending-box)
        (%pi-app-editor-set! app ed)
        ;; 若未指定自定义 handler，启动后台 (liii go) 独立工作协程
        ;; （abort-chan 供 worker 探测主线程 ESC 中断信号）
        (unless handler
          (go (pi-tui-agent-worker jobs-chan event-chan abort-chan))
        ) ;unless
        ;; 同理：未注入同步 bash 执行器时，启动后台 bash 工作协程
        (unless bash-handler
          (go (pi-bash-worker bash-jobs-chan event-chan))
        ) ;unless
        app
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
