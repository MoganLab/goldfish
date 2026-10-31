;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; /thinking 命令：直接设置、选择器交互、取消与默认级别保存
(define-library (liii pi code modes interactive app-thinking)
  (export pi-app-thinking-command? pi-app-submit-thinking!
    pi-app-close-selector! pi-app-cycle-thinking!
    pi-app-apply-thinking-level-ui! pi-app-toggle-thinking!
  ) ;export
  (import (scheme base)
    (scheme char)
    (liii go)
    (liii string)
    (liii list)
    (liii tui)
    (liii pi code modes interactive state)
    (liii pi code modes interactive render)
    (liii pi code modes interactive views)
    (liii pi code modes interactive footer)
    (liii pi code modes interactive thinking)
    (liii pi code modes interactive thinking-selector)
    (liii pi code modes interactive app-bash)
  ) ;import
  (begin

    (define (pi-app-thinking-command? text)
      (or (string=? text "/thinking") (string-starts? text "/thinking "))
    ) ;define

    (define (pi-app-close-selector! app)
      (%pi-app-active-selector-set! app #f)
      (editor-set-focused! (pi-app-editor app) #t)
      (pi-app-render! app)
    ) ;define

    ;; 记录思考等级切换状态到会话文件：与 /model 相同，投递 session_state
    ;; 任务由后台 worker 落盘；投递失败只提示警告卡片，不回滚界面状态
    (define (%record-thinking-change! app level)
      (catch #t
        (lambda ()
          (chan-send! (pi-app-jobs-chan app)
            (list 'session_state "thinking_level_change" (list (cons "level" level)))
            1000
          ) ;chan-send!
        ) ;lambda
        (lambda (tag . args)
          (container-add! (%pi-app-chat app)
            (make-tool-card "thinking" '() 'error "Failed to record thinking level change")
          ) ;container-add!
        ) ;lambda
      ) ;catch
    ) ;define

    ;; 只更新界面的思考等级（状态、footer 与编辑器边框）：
    ;; 不写状态条目、不出提示卡片；供 /thinking 与 /resume 共用
    (define (pi-app-apply-thinking-level-ui! app level)
      (%pi-app-thinking-level-set! app level)
      (pi-footer-set-thinking-level! (%pi-app-footer app) level)
      (editor-set-border-color! (pi-app-editor app) (pi-thinking-border-color level))
    ) ;define

    ;; 设置思考等级的唯一落点：选择器确认、/thinking 参数与 shift+tab 循环
    ;; 均经此生效；仅在与旧值不同且当前模型支持思考时记录一次（off 照常记录）
    (define (%apply-thinking-level! app level)
      (let ((old (pi-app-thinking-level app)))
        (pi-app-apply-thinking-level-ui! app level)
        (when (and (pi-app-supports-thinking? app) (not (equal? old level)))
          (%record-thinking-change! app level)
        ) ;when
      ) ;let
    ) ;define

    (define (pi-app-cycle-thinking! app)
      (if (pi-app-supports-thinking? app)
        (let ((next (pi-next-thinking-level (pi-app-thinking-level app)
                      (pi-app-available-thinking-levels app)
                    ) ;pi-next-thinking-level
              ) ;next
             ) ;
          (%apply-thinking-level! app next)
          next
        ) ;let
        #f
      ) ;if
    ) ;define

    (define (%select-thinking-level! app level persist?)
      (catch #t
        (lambda ()
          (%apply-thinking-level! app level)
          (when persist?
            (let ((save (%pi-app-on-save-thinking-level app)))
              (when (procedure? save)
                (save level)
              ) ;when
            ) ;let
            (%pi-app-default-thinking-level-set! app level)
          ) ;when
          (%pi-app-active-selector-set! app #f)
          (editor-set-focused! (pi-app-editor app) #t)
          (container-add! (%pi-app-chat app)
            (make-assistant-card (string-append (if persist? "Default thinking level: " "Thinking level: ")
                                   level
                                 ) ;string-append
            ) ;make-assistant-card
          ) ;container-add!
          (pi-app-render! app)
        ) ;lambda
        (lambda (tag . args)
          (%pi-app-active-selector-set! app #f)
          (editor-set-focused! (pi-app-editor app) #t)
          (container-add! (%pi-app-chat app)
            (make-tool-card "thinking" '() 'error "Failed to set thinking level")
          ) ;container-add!
          (pi-app-render! app)
        ) ;lambda
      ) ;catch
    ) ;define

    (define (%show-thinking-selector! app)
      (editor-set-focused! (pi-app-editor app) #f)
      (%pi-app-active-selector-set! app
        (make-thinking-selector (pi-app-thinking-level app)
          (pi-app-available-thinking-levels app)
          (pi-app-default-thinking-level app)
          (lambda (level) (%select-thinking-level! app level #f))
          (lambda () (pi-app-close-selector! app))
          (lambda (level) (%select-thinking-level! app level #t))
        ) ;make-thinking-selector
      ) ;%pi-app-active-selector-set!
      (pi-app-render! app)
    ) ;define

    (define (pi-app-submit-thinking! app text)
      (editor-set-text! (pi-app-editor app) "")
      (let* ((query (string-trim-both (substring text 9)))
             (available (pi-app-available-thinking-levels app))
            ) ;
        (if (string-null? query)
          (%show-thinking-selector! app)
          (let* ((normalized (string-downcase query))
                 (level
                   (find (lambda (candidate) (string=? (string-downcase candidate) normalized))
                     available
                   ) ;find
                 ) ;level
                ) ;
            (if level
              (%select-thinking-level! app level #f)
              (begin
                (container-add! (%pi-app-chat app)
                  (make-tool-card "thinking"
                    '()
                    'error
                    (string-append "Unknown thinking level \""
                      query
                      "\". Available levels: "
                      (string-join available ", ")
                      "."
                    ) ;string-append
                  ) ;make-tool-card
                ) ;container-add!
                (pi-app-render! app)
              ) ;begin
            ) ;if
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    ;; Ctrl+T 全局切换思考块显隐（对齐上游 toggleThinkingBlockVisibility）：
    ;; 翻转标志 → 已有卡片即时折叠/展开 → 持久化回调；
    ;; 不产生任何状态提示卡片，折叠/展开的视觉变化即反馈；
    ;; 新建卡片在 app-events 的 %pi-app-handle-thinking-line! 中继承新状态
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
        (let ((save (%pi-app-on-save-hide-thinking app)))
          (when save
            (save next)
          ) ;when
        ) ;let
        next
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
