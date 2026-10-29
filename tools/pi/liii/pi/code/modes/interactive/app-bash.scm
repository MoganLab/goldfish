;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; bash 编排层：! 命令的卡片上屏、!! 排除与上下文注入（对齐 pi
;; handleBashCommand / recordBashResult），从 app.scm 拆出以控制单文件行数
(define-library (liii pi code modes interactive app-bash)
  (export pi-app-run-bash! pi-app-toggle-tool-output! %pi-app-record-bash!)
  (import (scheme base)
    (liii go)
    (liii tui)
    (liii pi code modes interactive state)
    (liii pi code modes interactive render)
    (liii pi code modes interactive bash)
    (liii pi code modes interactive views)
    (liii pi code modes interactive status-card)
  ) ;import
  (begin

    (define (%bash-opts-excluded? opts)
      (cond ((null? opts) #f)
            ((and (eq? (car opts) :excluded?) (pair? (cdr opts))) (cadr opts))
            (else (%bash-opts-excluded? (cddr opts)))
      ) ;cond
    ) ;define

    ;; ! 命令结果注入上下文（对齐 pi recordBashResult）：经 record-sink 回调；
    ;; 默认投递 (bash_record cmd text code) 任务到 jobs-chan，由后台 worker
    ;; 写入会话文件（忙时在通道排队，对齐 pi 流式期间延迟注入的语义）
    (define (%pi-app-record-bash! app command text code excluded?)
      (unless excluded?
        (let ((sink (%pi-app-record-sink app)))
          (when (procedure? sink)
            (sink command text code)
          ) ;when
        ) ;let
      ) ;unless
    ) ;define

    ;; 执行 bash 命令并上屏卡片（对齐 pi handleBashCommand）：同步 handler
    ;; （测试接缝）直接回填；生产路径派发到后台 worker，完成后经
    ;; event-chan 发 bash_complete 事件由 poll-events! 回填。
    ;; excluded?（!! 前缀）时仅显示不注入上下文（对齐 pi excludeFromContext）
    (define (pi-app-run-bash! app command . opts)
      (let* ((excluded? (%bash-opts-excluded? opts))
             (ed (pi-app-editor app))
             (cards (%pi-app-bash-cards app))
             (id (+ 1 (length cards)))
             (card (make-bash-card command
                     :expanded? (pi-app-tools-expanded? app)
                     :excluded? excluded?
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
              (%pi-app-record-bash! app command text code excluded?)
            ) ;let-values
            (chan-send! (pi-app-bash-jobs-chan app) (list id command excluded?) 1000)
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    ;; Ctrl+O 全局切换 bash 卡片展开/折叠（对齐 pi toggleToolOutputExpansion）
    ;; 同时覆盖 ! 命令卡片与模型工具调用卡片；切换后在 chat 底部显示
    ;; "Tool output: expanded/collapsed" 状态行（对齐上游 showStatus，
    ;; 已有状态卡片时原地更新消息，连续切换不堆积）。
    ;; 返回切换后的展开状态
    (define (%pi-app-show-status! app message)
      (let* ((children (container-children (%pi-app-chat app)))
             (last (and (pair? children) (car (reverse children))))
            ) ;
        (if (status-card? last)
          (status-card-set-message! last message)
          (container-add! (%pi-app-chat app) (make-status-card message))
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-app-toggle-tool-output! app)
      (let ((next (not (pi-app-tools-expanded? app))))
        (if (eq? next (pi-app-tools-expanded? app))
          next
          (begin
            (%pi-app-tools-expanded?-set! app next)
            (for-each (lambda (child)
                        (when (bash-card-component? child)
                          (bash-card-set-expanded! child next)
                        ) ;when
                        (when (tool-card-component? child)
                          (tool-card-set-expanded! child next)
                        ) ;when
                      ) ;lambda
              (container-children (%pi-app-chat app))
            ) ;for-each
            (%pi-app-show-status! app
              (string-append "Tool output: " (if next "expanded" "collapsed"))
            ) ;%pi-app-show-status!
            next
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
