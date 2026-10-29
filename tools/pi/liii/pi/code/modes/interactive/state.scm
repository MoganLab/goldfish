;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; <pi-app> 状态层：record 定义与全部字段访问器/修改器集中导出，
;; 供 app（构造与调度）、render（渲染）等子模块共同使用，避免循环依赖
(define-library (liii pi code modes interactive state)
  (export %make-pi-app pi-app? pi-app-term %pi-app-term pi-app-screen
    %pi-app-doc %pi-app-chat %pi-app-pending-box pi-app-editor
    %pi-app-editor-set! %pi-app-footer pi-app-queue %pi-app-handler
    pi-app-active-selector %pi-app-active-selector-set!
    pi-app-slash-command-menu %pi-app-slash-command-menu-set! pi-app-jobs-chan
    pi-app-event-chan pi-app-abort-chan pi-app-working pi-app-thinking-level
    %pi-app-thinking-level-set! pi-app-supports-thinking?
    pi-app-available-thinking-levels pi-app-default-thinking-level
    %pi-app-default-thinking-level-set! %pi-app-on-save-thinking-level
    pi-app-bash-mode? %pi-app-bash-mode?-set! pi-app-tools-expanded?
    %pi-app-tools-expanded?-set! %pi-app-bash-cards %pi-app-bash-cards-set!
    %pi-app-tool-cards %pi-app-tool-cards-set! pi-app-bash-jobs-chan
    %pi-app-bash-handler %pi-app-record-sink pi-app-session-name
    %pi-app-session-name-set! %pi-editor-top-border pi-app-provider
    %pi-app-provider-set! pi-app-model %pi-app-model-set! pi-app-models
    %pi-app-default-model %pi-app-on-save-model %pi-app-supports-thinking?-set!
    %pi-app-available-thinking-levels-set! pi-app-resume-sessions
    %pi-app-resume-sessions-set! pi-app-no-session? pi-app-scroll-view
    pi-app-wheel-accelerator pi-app-steering-chan pi-app-follow-up-chan
    %pi-app-steering-pending %pi-app-steering-pending-set!
    %pi-app-follow-up-pending %pi-app-follow-up-pending-set!
  ) ;export
  (import (scheme base))
  (begin

    ;; 编辑器顶边框：开启时占 1 行，渲染时光标行需跳过该行
    (define %pi-editor-top-border #t)

    (define-record-type <pi-app>
      (%make-pi-app term screen doc chat pending-box editor active-selector
        slash-command-menu footer queue handler jobs-chan event-chan abort-chan
        working thinking-level supports-thinking available-thinking-levels
        default-thinking-level on-save-thinking-level bash-mode? tools-expanded?
        bash-cards bash-jobs-chan bash-handler record-sink session-name provider
        model models default-model on-save-model tool-cards resume-sessions
        no-session? scroll-view wheel-accelerator steering-chan follow-up-chan
        steering-pending follow-up-pending
      ) ;%make-pi-app
      pi-app?
      (term pi-app-term)
      (screen pi-app-screen)
      (doc %pi-app-doc)
      (chat %pi-app-chat)
      (pending-box %pi-app-pending-box)
      (editor pi-app-editor %pi-app-editor-set!)
      (active-selector pi-app-active-selector %pi-app-active-selector-set!)
      (slash-command-menu pi-app-slash-command-menu %pi-app-slash-command-menu-set!)
      (footer %pi-app-footer)
      (queue pi-app-queue)
      (handler %pi-app-handler)
      (jobs-chan pi-app-jobs-chan)
      (event-chan pi-app-event-chan)
      ;; ESC 中断信号通道：主线程发送，后台 worker 接收探测
      (abort-chan pi-app-abort-chan)
      (working pi-app-working)
      (thinking-level pi-app-thinking-level %pi-app-thinking-level-set!)
      (supports-thinking pi-app-supports-thinking? %pi-app-supports-thinking?-set!)
      (available-thinking-levels pi-app-available-thinking-levels
        %pi-app-available-thinking-levels-set!
      ) ;available-thinking-levels
      (default-thinking-level pi-app-default-thinking-level
        %pi-app-default-thinking-level-set!
      ) ;default-thinking-level
      (on-save-thinking-level %pi-app-on-save-thinking-level)
      ;; bash 模式：编辑器文本以 ! 开头时边框变绿（对齐 pi isBashMode）
      (bash-mode? pi-app-bash-mode? %pi-app-bash-mode?-set!)
      ;; Ctrl+O 全局工具输出展开状态（对齐 pi toolOutputExpanded）
      (tools-expanded? pi-app-tools-expanded? %pi-app-tools-expanded?-set!)
      ;; id → bash 卡片的关联表（后台事件回填用）
      (bash-cards %pi-app-bash-cards %pi-app-bash-cards-set!)
      (bash-jobs-chan pi-app-bash-jobs-chan)
      ;; 测试接缝：注入同步 bash 执行器 (lambda (cmd) (values text exit-code))
      (bash-handler %pi-app-bash-handler)
      ;; ! 命令结果注入会话上下文的回调 (lambda (command text code))；
      ;; 默认投递 (bash_record cmd text code) 任务到 jobs-chan 由后台 worker 落盘
      (record-sink %pi-app-record-sink)
      ;; 模型工具调用 id → 可更新卡片；start/update/end 在同一组件上原地推进
      (tool-cards %pi-app-tool-cards %pi-app-tool-cards-set!)
      (session-name pi-app-session-name %pi-app-session-name-set!)
      ;; /model 会话内切换：当前 provider/model，随下一条消息生效
      (provider pi-app-provider %pi-app-provider-set!)
      (model pi-app-model %pi-app-model-set!)
      ;; 可用模型列表与默认模型（settings），选择器数据源与 · default 标记
      (models pi-app-models)
      (default-model %pi-app-default-model)
      ;; Ctrl+S 持久化回调 (lambda (provider model-id))
      (on-save-model %pi-app-on-save-model)
      ;; /resume 会话数据源：无参调用返回 (values current-sessions all-thunk)，
      ;; 由 cli.scm 复用 pi-load-resume-sessions 提供；#f 表示不可用
      (resume-sessions pi-app-resume-sessions %pi-app-resume-sessions-set!)
      ;; --no-session 模式：会话持久化已禁用，/resume 明确拒绝切换
      (no-session? pi-app-no-session?)
      ;; 全屏模式的对话记录视口：可滚动，支持 follow 末尾
      (scroll-view pi-app-scroll-view)
      ;; 鼠标滚轮滚动加速器：把滚轮事件换算成滚动行数
      (wheel-accelerator pi-app-wheel-accelerator)
      ;; steering / follow-up 注入通道（主线程写入，后台 worker 轮询消费）
      (steering-chan pi-app-steering-chan)
      (follow-up-chan pi-app-follow-up-chan)
      ;; 待处理消息显示列表（主线程维护，与通道内容 1:1 同步；
      ;; worker 消费时经 user_message 事件逐条移除）
      (steering-pending %pi-app-steering-pending %pi-app-steering-pending-set!)
      (follow-up-pending %pi-app-follow-up-pending %pi-app-follow-up-pending-set!)
    ) ;define-record-type

    (define %pi-app-term pi-app-term)

  ) ;begin
) ;define-library
