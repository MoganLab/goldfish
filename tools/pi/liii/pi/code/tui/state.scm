;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; <pi-app> 状态层：record 定义与全部字段访问器/修改器集中导出，
;; 供 app（构造与调度）、render（渲染）等子模块共同使用，避免循环依赖
(define-library (liii pi code tui state)
  (export %make-pi-app pi-app? pi-app-term %pi-app-term pi-app-screen
    %pi-app-doc %pi-app-chat %pi-app-pending-box pi-app-editor
    %pi-app-editor-set! %pi-app-footer pi-app-queue %pi-app-handler
    pi-app-jobs-chan pi-app-event-chan pi-app-working pi-app-thinking-level
    %pi-app-thinking-level-set! pi-app-supports-thinking? pi-app-bash-mode?
    %pi-app-bash-mode?-set! pi-app-tools-expanded? %pi-app-tools-expanded?-set!
    %pi-app-bash-cards %pi-app-bash-cards-set! pi-app-bash-jobs-chan
    %pi-app-bash-handler %pi-editor-top-border
  ) ;export
  (import (scheme base))
  (begin

    ;; 编辑器顶边框：开启时占 1 行，渲染时光标行需跳过该行
    (define %pi-editor-top-border #t)

    (define-record-type <pi-app>
      (%make-pi-app term screen doc chat pending-box editor footer queue handler
        jobs-chan event-chan working thinking-level supports-thinking bash-mode?
        tools-expanded? bash-cards bash-jobs-chan bash-handler
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

  ) ;begin
) ;define-library
