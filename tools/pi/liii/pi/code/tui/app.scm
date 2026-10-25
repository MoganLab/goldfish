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
    pi-app-poll-events! pi-app-working pi-app-working-tick!
    pi-app-thinking-level pi-app-supports-thinking? pi-app-cycle-thinking!
    pi-app-bash-mode? pi-app-tools-expanded? pi-app-toggle-tool-output!
    pi-app-bash-jobs-chan pi-app-run-bash!
  ) ;export
  (import (scheme base)
    (liii go)
    (liii time)
    (liii date-time)
    (liii string)
    (liii list)
    (liii tui)
    (liii pi agent queue)
    (liii pi code tui views)
    (liii pi code tui thinking)
    (liii pi code tui working)
    (liii pi code tui worker)
    (liii pi code tui bash)
    (liii pi code tui common)
    (liii pi code tui header)
    (liii pi code tui footer)
  ) ;import
  (begin

    ;; 编辑器顶边框：开启时占 1 行，渲染时光标行需跳过该行
    (define %pi-editor-top-border #t)

    (define-record-type <pi-app>
      (%make-pi-app term screen doc chat pending-box editor footer queue handler
        jobs-chan event-chan working thinking-level supports-thinking
        bash-mode? tools-expanded? bash-cards bash-jobs-chan bash-handler
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
      (working pi-app-working)
      (thinking-level pi-app-thinking-level %pi-app-thinking-level-set!)
      (supports-thinking pi-app-supports-thinking?)
      ;; bash 模式：编辑器文本以 ! 开头时边框变绿（对齐 pi isBashMode）
      (bash-mode? pi-app-bash-mode? %pi-app-bash-mode?-set!)
      ;; Ctrl+O 全局工具输出展开状态（对齐 pi toolOutputExpanded）
      (tools-expanded? pi-app-tools-expanded? %pi-app-tools-expanded?-set!)
      ;; id → bash 卡片的关联表（后台事件回填用）
      (bash-cards %pi-app-bash-cards %pi-app-bash-cards-set!)
      (bash-jobs-chan pi-app-bash-jobs-chan)
      ;; 测试接缝：注入同步 bash 执行器 (lambda (cmd) (values text exit-code))
      (bash-handler %pi-app-bash-handler)
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
                 (append-map (lambda (item)
                               (if (string-contains? item "\n") (string-split item "\n") (list item))
                             ) ;lambda
                   raw-doc-lines
                 ) ;append-map
               ) ;doc-lines
               (ed (pi-app-editor app))
               ;; busy 时编辑器上边框替换为 Working 动画行（行数不变）
               (ed-lines
                 (let ((raw (component-render ed w)))
                   (if (and (pi-queue-busy? q) (pair? raw) %pi-editor-top-border)
                     (cons (working-border-line (pi-app-working app)
                             w
                             (pi-thinking-border-color (pi-app-thinking-level app))
                           ) ;working-border-line
                       (cdr raw)
                     ) ;cons
                     raw
                   ) ;if
                 ) ;let
               ) ;ed-lines
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

    ;; 判断字符串是否以指定字符开头（避免 string-prefix? 跨库歧义）
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

    ;; 执行 bash 命令并上屏卡片（对齐 pi handleBashCommand）：
    ;; 同步 handler（测试接缝）直接回填；生产路径派发到后台 worker，
    ;; 完成后经 event-chan 发 bash_complete 事件由 poll-events! 回填
    (define (pi-app-run-bash! app command)
      (let* ((ed (pi-app-editor app))
             (cards (%pi-app-bash-cards app))
             (id (+ 1 (length cards)))
             (card (make-bash-card command
                     :expanded? (pi-app-tools-expanded? app)
                   ) ;make-bash-card
             ) ;card
            ) ;
        ;; 清空输入框（触发 on-change → 退出 bash 模式、恢复边框颜色）
        (editor-set-text! ed "")
        (%pi-app-bash-cards-set! app (cons (cons id card) cards))
        (container-add! (%pi-app-chat app) card)
        (pi-app-render! app)
        (let ((handler (%pi-app-bash-handler app)))
          (if handler
            (let-values (((text code) (handler command)))
              (bash-card-set-output! card text)
              (bash-card-set-complete! card code)
              (pi-app-render! app)
            ) ;let-values
            (chan-send! (pi-app-bash-jobs-chan app) (list id command) 1000)
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    ;; Ctrl+O 全局切换 bash 卡片展开/折叠（对齐 pi toggleToolOutputExpansion）
    ;; 返回切换后的展开状态
    (define (pi-app-toggle-tool-output! app)
      (let ((next (not (pi-app-tools-expanded? app))))
        (%pi-app-tools-expanded?-set! app next)
        (for-each
          (lambda (child)
            (when (bash-card-component? child)
              (bash-card-set-expanded! child next)
            ) ;when
          ) ;lambda
          (pi-app-chat app)
        ) ;for-each
        next
      ) ;let
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
                    ((bash_complete)
                     ;; 后台 bash 执行完成：按 id 回填卡片输出与退出码
                     (let ((id (cadr ev))
                           (text (caddr ev))
                           (code (cadddr ev))
                          ) ;
                       (let ((entry (assq id (%pi-app-bash-cards app))))
                         (when entry
                           (bash-card-set-output! (cdr entry) text)
                           (bash-card-set-complete! (cdr entry) code)
                         ) ;when
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

    ;; 用户提交动作：! 前缀拦截为 bash 模式（不发给模型）；
    ;; 其余消息统一先进入等待队列并清空输入框；
    ;; 队列忙时立即渲染出 [Queued] 提示，闲时由 drain-queue! 渲染唯一一帧
    (define (pi-app-submit! app text)
      (let ((clean (string-trim text)))
        (unless (string-null? clean)
          ;; bash 模式：!cmd 直接经 bash 执行（对齐 pi 的 ! 前缀行为）；
          ;; 命令为空（仅 "!"）时回退为普通消息提交（对齐 pi 的 fall-through）
          (if (and (%starts-with-char? clean #\!)
                   (not (string-null? (string-trim (substring clean 1)))))
            (pi-app-run-bash! app (string-trim (substring clean 1)))
            (let ((q (pi-app-queue app)) (ed (pi-app-editor app)))
              (pi-queue-enqueue! q clean)
              (editor-set-text! ed "")
              (when (pi-queue-busy? q)
                (pi-app-render! app)
              ) ;when
              (pi-app-drain-queue! app)
            ) ;let
          ) ;if
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
             (tl (or (%keyword-value :thinking-level args #f) "off"))
             (supports? (%keyword-value :supports-thinking args #t))
             (footer (make-pi-footer
                       :branch            branch
                       :provider          (%keyword-value :provider args #f)
                       :model             (%keyword-value :model args #f)
                       :thinking-level    tl
                       :supports-thinking supports?
                     ) ;make-pi-footer
             ) ;footer
             (queue (make-pi-message-queue))
             (jobs-chan (make-chan 16))
             (event-chan (make-chan 32))
             (bash-jobs-chan (make-chan 16))
             (bash-handler-opt (%keyword-value :bash-handler args #f))
             (app (%make-pi-app actual-term
                    screen
                    doc
                    chat
                    pending-box
                    #f
                    footer
                    queue
                    handler-opt
                    jobs-chan
                    event-chan
                    (make-working-indicator)
                    tl
                    supports?
                    #f
                    #f
                    '()
                    bash-jobs-chan
                    bash-handler-opt
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
                   :on-change     (lambda (ed* txt)
                                    (%pi-app-editor-on-change app ed* txt)
                                  ) ;lambda
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
        ;; 同理：未注入同步 bash 执行器时，启动后台 bash 工作协程
        (unless bash-handler-opt
          (go (pi-bash-worker bash-jobs-chan event-chan))
        ) ;unless
        app
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
