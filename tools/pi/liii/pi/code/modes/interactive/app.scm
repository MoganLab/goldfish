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
    pi-app-hide-thinking?
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
    ;; thinking 选择器优先，与既有主循环行为一致；
    ;; alt+enter：排队 follow-up（busy 时等运行结束，空闲时等同回车），
    ;; 对齐上游 app.message.followUp 键位
    (define (pi-app-handle-input! app key)
      (let ((selector (pi-app-active-selector app))
            (menu (pi-app-slash-command-menu app))
            (ed (pi-app-editor app))
           ) ;
        (cond (selector (component-handle-input selector key))
              ((and (eq? key 'alt-enter) (not menu))
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
                    ((click) (when (pi-app-dispatch-mouse! app ev) (set! handled? #t)))
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
        :cursor-style     'hardware
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
               (session-name #f)
               (models '())
               (default-model #f)
               (on-save-model #f)
               (session-file #f)
               (resume-sessions #f)
               (no-session? #f)
               (hide-thinking #f)
               (on-save-hide-thinking #f)
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
             (default-level (or default-thinking-level tl))
             (footer (make-pi-footer
                       :branch            branch
                       :provider          provider
                       :model             model
                       :thinking-level    tl
                       :supports-thinking supports-thinking
                       :session-name      session-name
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
                  ) ;%make-pi-app
             ) ;app
             (ed (%make-pi-editor app tl))
            ) ;
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
