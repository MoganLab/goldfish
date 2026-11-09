;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes interactive app)
  (export make-pi-app pi-app? pi-app-term pi-app-screen pi-app-editor
    pi-app-queue pi-app-chat pi-app-render! pi-app-submit!
    pi-app-submit-initial! pi-app-drain-queue! pi-app-add-user-message!
    pi-app-add-tool-result! pi-app-add-assistant-text! pi-app-add-retry-message!
    pi-app-jobs-chan pi-app-event-chan pi-app-abort-chan pi-app-poll-events!
    pi-app-working pi-app-working-tick! pi-app-thinking-level
    pi-app-supports-thinking? pi-app-cycle-thinking! pi-app-bash-mode?
    pi-app-active-selector pi-app-slash-command-menu pi-app-handle-input!
    pi-app-default-thinking-level pi-app-tools-expanded?
    pi-app-toggle-tool-output! pi-app-bash-jobs-chan pi-app-run-bash!
    pi-app-provider pi-app-model pi-app-models %pi-app-job-payload
    pi-app-available-thinking-levels pi-app-resume-command?
    pi-app-submit-resume! pi-app-handle-resume-event! pi-app-scroll-view
    pi-app-scroll-up! pi-app-scroll-down! pi-app-scroll-top!
    pi-app-scroll-bottom! pi-app-scroll-line-up! pi-app-scroll-line-down!
    pi-app-frame-lines pi-app-transcript-lines pi-app-wheel-accelerator
    pi-app-handle-mouse! pi-app-handle-mouse-events! pi-app-toggle-thinking!
    pi-app-hide-thinking? pi-app-selection pi-app-selection-set!
    pi-app-selection-clear! pi-app-selection-active? pi-app-tokens
  ) ;export
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii go)
    (liii time)
    (liii date-time)
    (liii string)
    (liii list)
    (liii tui)
    (liii pi agent queue)
    (liii pi code core session-stats)
    (liii pi code core context-usage)
    (liii pi code modes interactive state)
    (liii pi code modes interactive render)
    (liii pi code modes interactive thinking)
    (liii pi code modes interactive working)
    (liii pi code modes interactive worker)
    (liii pi code modes interactive bash)
    (liii pi code modes interactive app-bash)
    (liii pi code modes interactive app-events)
    (liii pi code modes interactive app-name)
    (liii pi code modes interactive app-session)
    (liii pi code modes interactive app-thinking)
    (liii pi code modes interactive app-model)
    (liii pi code modes interactive app-resume)
    (liii pi code modes interactive app-submit)
    (liii pi code modes interactive app-mouse)
    (liii pi code modes interactive common)
    (liii pi code modes interactive header)
    (liii pi code modes interactive footer)
  ) ;import
  (begin

    ;; bash 编排（! 执行、!! 排除、结果注入上下文、Ctrl+O 展开/折叠）已拆至
    ;; (liii pi code modes interactive app-bash)，此处经 import 再导出保持对外接口不变
    ;; 消息卡片、队列调度与后台事件处理位于 app-events，同样经 import 再导出

    ;; Working 动画：busy 时按 80ms 节拍重绘，返回本拍是否渲染
    (define (pi-app-working-tick! app)
      (let ((q (pi-app-queue app)) (ind (pi-app-working app)))
        (if (not (pi-queue-busy? q))
          (begin
            (working-reset! ind)
            #f
          ) ;begin
          (let ((now (current-monotonic-millis)))
            (cond ((not (working-last-tick ind))
                   ;; busy 首拍：整帧渲染（布局可能变化，需完整重算），从 ⠋ 起始
                   (working-reset! ind)
                   (working-last-tick-set! ind now)
                   (pi-app-render! app)
                   #t
                  ) ;
                  ((>= (- now (working-last-tick ind)) %working-frame-interval-ms)
                   (working-advance! ind)
                   (working-last-tick-set! ind now)
                   ;; 轻量路径：busy 期间布局不变（每拍仅 spinner 帧变化），
                   ;; 基于上帧仅替换 Working 边框行，避免每拍重算整帧
                   ;; （长会话整帧重算成本可达 ~16ms，导致空闲 CPU ~20%）。
                   ;; 回退条件：选择器激活（编辑器行被替换，行定位不适用）
                   ;; 或尚无缓存帧（首次 busy 前未渲染过）
                   (if (%pi-app-render-working-border! app) #t (begin (pi-app-render! app) #t))
                  ) ;
                  (else #f)
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%pi-app-complete-command! app menu ed)
      (let ((item (select-list-selected-item menu)))
        (when item
          (let ((completed (string-append "/" (cdr (assoc 'value item)) " ")))
            (editor-set-text! ed completed)
            (editor-set-cursor! ed 0 (string-length completed))
          ) ;let
        ) ;when
      ) ;let
    ) ;define

    (define (%pi-app-submit-command! app menu)
      (let ((item (select-list-selected-item menu)))
        (when item
          (%pi-app-slash-command-menu-set! app #f)
          (pi-app-submit! app (string-append "/" (cdr (assoc 'value item))))
        ) ;when
      ) ;let
    ) ;define

    ;; 输入分发：命令菜单激活时处理选择键，其余输入仍交给编辑器；
    ;; thinking 选择器优先，与既有主循环行为一致。
    ;; alt+enter / ctrl-q 均排队 follow-up：ctrl-q 对齐上游
    ;; app.message.followUp 在 Windows/WSL 下的默认键位，因为 Windows
    ;; Terminal 等终端把 Alt+Enter 截为「全屏切换」，按键送不到进程
    ;; （任务 1317）。
    (define (pi-app-handle-input! app key)
      (let ((selector (pi-app-active-selector app))
            (menu (pi-app-slash-command-menu app))
            (ed (pi-app-editor app))
           ) ;
        (cond (selector (component-handle-input selector key))
              ((and (memq key '(alt-enter ctrl-q)) (not menu))
               (let ((text (string-trim (editor-expanded-text ed))))
                 (unless (string-null? text)
                   ;; 与编辑器原生提交同步：入历史 + 清空输入框
                   (history-push! (editor-history ed) text)
                   (editor-set-text! ed "")
                   (pi-app-submit! app text 'follow-up)
                 ) ;unless
                 #t
               ) ;let
              ) ;
              (menu (case key
                     ((escape) (%pi-app-slash-command-menu-set! app #f) #t)
                     ((up down) (component-handle-input menu key))
                     ((tab) (%pi-app-complete-command! app menu ed) #t)
                     ((enter) (%pi-app-submit-command! app menu) #t)
                     (else (component-handle-input ed key))
                    ) ;case
              ) ;menu
              (else (component-handle-input ed key))
        ) ;cond
      ) ;let
    ) ;define

    ;; 鼠标事件处理：滚轮上下滚动对话记录；点击派发给 doc 组件树
    ;; （由卡片自身回调完成点击切换展开）
    ;; 返回 #t 表示事件已被消费
    (define (pi-app-handle-mouse! app event)
      (pi-app-handle-mouse-events! app (list event))
    ) ;define

    ;; 批量鼠标事件处理：把连续到达的滚轮事件合并为一次滚动（只滚动一次），
    ;; 点击事件照旧逐个派发。
    ;;
    ;; 合并的意义：滚轮事件到达间隔（约 5~10ms）远小于单次滚动的渲染耗时，
    ;; 逐个处理会让输入队列不断积压，表现为“滑几下就越滚越慢”；
    ;; 合并后一次滚动处理完所有已到达事件，视图不再滞后。
    ;; 返回 #t 表示需要重绘。
    (define (pi-app-handle-mouse-events! app events)
      (let ((acc (pi-app-wheel-accelerator app)) (delta 0) (handled? #f))
        (for-each
          (lambda (ev)
            ;; 用事件自带的到达时刻而非处理时刻：合并批次内各事件的间隔
            ;; 是滚轮加速的唯一依据，统一取处理时刻会把间隔记为 0 而抹平加速
            ;; （Issue #1382）。无时间戳的旧事件回退到处理时刻，行为不变。
            (let ((now (or (mouse-event-time ev) (current-monotonic-millis))))
              (case (mouse-event-type ev)
                    ((wheel-up)
                     (set! delta (- delta (wheel-scroll-next! acc -1 now)))
                     (set! handled? #t)
                    ) ;
                    ((wheel-down)
                     (set! delta (+ delta (wheel-scroll-next! acc 1 now)))
                     (set! handled? #t)
                    ) ;
                    ((press drag release)
                     (when (pi-app-handle-selection-mouse! app ev)
                       (set! handled? #t)
                     ) ;when
                    ) ;
                    ((click)
                     (when (pi-app-handle-selection-mouse! app ev)
                       (set! handled? #t)
                     ) ;when
                     (when (pi-app-dispatch-mouse! app ev)
                       (set! handled? #t)
                     ) ;when
                    ) ;
                    (else #f)
              ) ;case
            ) ;let
          ) ;lambda
          events
        ) ;for-each
        (unless (zero? delta)
          (scroll-view-scroll-by! (pi-app-scroll-view app) delta)
        ) ;unless
        handled?
      ) ;let
    ) ;define

    (define (pi-app-submit-initial! app initial-msg rest-msgs)
      (unless (string-null? initial-msg)
        (pi-app-submit! app initial-msg)
      ) ;unless
      (for-each (lambda (msg) (pi-app-submit! app msg)) rest-msgs)
    ) ;define

    (define (%make-pi-record-sink record-sink jobs-chan)
      (or record-sink
        (lambda (command text code)
          (catch #t
            (lambda () (chan-send! jobs-chan (list 'bash_record command text code) 1000))
            (lambda args #f)
          ) ;catch
        ) ;lambda
      ) ;or
    ) ;define

    (define (%make-pi-editor app thinking-level)
      (make-editor ""
        :multiline        #t
        :submit-on-enter? #t
        :top-border       %pi-editor-top-border
        :bottom-border    #t
        :border-color     (pi-thinking-border-color thinking-level)
        :focused          #t
        :on-submit        (lambda (self txt) (pi-app-submit! app txt))
        :on-change (lambda (ed txt) (%pi-app-editor-on-change app ed txt))
      ) ;make-editor
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
               (available-thinking-levels #f)
               (default-thinking-level #f)
               (on-save-thinking-level #f)
               (model-thinking-levels '())
               (session-name #f)
               (models '())
               (default-model #f)
               (on-save-model #f)
               (session-file #f)
               (resume-sessions #f)
               (no-session? #f)
               (hide-thinking #f)
               (on-save-hide-thinking #f)
               (tokens #f)
               (context-window #f)
               (skills '())
               (enable-skill-commands #t)
             ) ;make-pi-app
      (let* ((actual-term (or terminal (make-standard-terminal)))
             ;; 全屏模式：备用屏整帧渲染，内容不随终端滚动
             (screen (make-alt-screen actual-term))
             (header (make-pi-header))
             (chat (make-container))
             (pending-box (make-container))
             (doc (make-container))
             (branch (detect-git-branch))
             (tl (or thinking-level "off"))
             (available-levels (or available-thinking-levels
                                 (if supports-thinking pi-thinking-levels '("off"))
                               ) ;or
             ) ;available-levels
             ;; 默认级别：settings.json 里的原始值（未配置时为 #f，由 medium 兜底）
             (default-level default-thinking-level)
             (actual-tokens
               (or tokens
                 (if (and session-file (string? session-file) (file-exists? session-file))
                   (pi-session-stats-tokens (pi-get-session-stats session-file))
                   (make-pi-session-token-stats 0 0 0 0)
                 ) ;if
               ) ;or
             ) ;actual-tokens
             (footer (make-pi-footer
                       :branch            branch
                       :provider          provider
                       :model             model
                       :thinking-level    tl
                       :supports-thinking supports-thinking
                       :session-name      session-name
                       ;; 上下文窗口：页脚左下角上下文占比的容量分母
                       :context-window context-window
                     ) ;make-pi-footer
             ) ;footer
             (queue (make-pi-message-queue))
             (jobs-chan (make-chan 16))
             (event-chan (make-chan 32))
             (abort-chan (make-chan 1))
             (bash-jobs-chan (make-chan 16))
             ;; steering / follow-up 注入通道：容量 16 与 jobs-chan 对齐
             (steering-chan (make-chan 16))
             (follow-up-chan (make-chan 16))
             (actual-record-sink (%make-pi-record-sink record-sink jobs-chan))
             (app (%make-pi-app actual-term
                    screen
                    doc
                    chat
                    pending-box
                    #f
                    #f
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
                    available-levels
                    default-level
                    on-save-thinking-level
                    model-thinking-levels
                    #f
                    #f
                    '()
                    bash-jobs-chan
                    bash-handler
                    actual-record-sink
                    session-name
                    (or provider "google")
                    (or model "gemini-3.8-flash")
                    (or models '())
                    default-model
                    on-save-model
                    '()
                    resume-sessions
                    (and no-session? #t)
                    (make-scroll-view doc)
                    (make-wheel-scroll-accelerator)
                    steering-chan
                    follow-up-chan
                    '()
                    '()
                    #f
                    (and hide-thinking #t)
                    on-save-hide-thinking
                    #f
                    #f
                    actual-tokens
                    #f
                    #f
                    skills
                    (and enable-skill-commands #t)
                  ) ;%make-pi-app
             ) ;app
             (ed (%make-pi-editor app tl))
            ) ;
        (when (> (pi-session-token-stats-total actual-tokens) 0)
          ;; 恢复的会话：按最后一条 assistant usage 换算上下文占比
          (pi-footer-set-token-stats! footer
            actual-tokens
            #f
            (pi-session-context-usage session-file (pi-footer-context-window footer))
          ) ;pi-footer-set-token-stats!
        ) ;when
        ;; 对话记录（页眉 + 消息卡片）作为可滚动视口内容；
        ;; 待发送队列卡片与输入区、页脚一起构成底部 dock
        (container-add! doc header)
        (container-add! doc chat)
        (%pi-app-editor-set! app ed)
        ;; 无自定义 handler 时启动后台 agent worker
        (unless handler
          (go (pi-tui-agent-worker jobs-chan event-chan abort-chan session-file
                steering-chan follow-up-chan
              ) ;pi-tui-agent-worker
          ) ;go
        ) ;unless
        ;; 无注入执行器时启动后台 bash worker
        (unless bash-handler
          (go (pi-bash-worker bash-jobs-chan event-chan))
        ) ;unless
        app
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
