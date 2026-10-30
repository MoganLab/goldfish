;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; /resume 命令：在运行中的 TUI 内复用 --resume 的会话选择器切换会话。
;; 前台：边界拒绝（忙碌、--no-session、无数据源）后以 active-selector
;; 打开 make-session-selector；确认后向 jobs-chan 投递 session_resume 任务，
;; 由后台 worker 串行切换 sfile-box，完成后经 event-chan 回填名称与提示。
(define-library (liii pi code modes interactive app-resume)
  (export pi-app-resume-command?
    pi-app-submit-resume!
    pi-app-handle-resume-event!
  ) ;export
  (import (scheme base)
    (liii go)
    (liii string)
    (liii list)
    (liii tui)
    (liii pi agent queue)
    (liii pi code modes interactive state)
    (liii pi code modes interactive render)
    (liii pi code modes interactive views)
    (liii pi code modes interactive footer)
    (liii pi code modes interactive thinking)
    (liii pi code modes interactive app-model)
    (liii pi code modes interactive app-thinking)
    (liii pi code modes interactive session-selector)
  ) ;import
  (begin

    ;; /resume 为无参斜杠命令：仅精确匹配 "/resume" 才触发会话切换，
    ;; "/resume xxx" 视为误用并提示用法（避免被当作普通消息发给模型）
    (define (pi-app-resume-command? text)
      (or (string=? text "/resume") (string-starts? text "/resume "))
    ) ;define

    ;; 拒绝提示：以错误卡片上屏，不打开选择器、不切换会话、不创建文件
    (define (%refuse! app text)
      (editor-set-text! (pi-app-editor app) "")
      (container-add! (%pi-app-chat app) (make-tool-card "resume" '() 'error text))
      (pi-app-render! app)
    ) ;define

    ;; 固定的拒绝文案（忙碌时提示稍后重试）
    (define %busy-message "Agent is busy; try /resume again later")

    ;; 打开会话选择器：沿用 --resume 的交互（Tab 切换范围、模糊搜索、
    ;; Ctrl+S 排序、Ctrl+N 命名过滤、Ctrl+P 显示路径），编辑器暂时失焦
    (define (%show-resume-selector! app)
      (catch #t
        (lambda ()
          (let-values (((current all) ((pi-app-resume-sessions app))))
            (editor-set-text! (pi-app-editor app) "")
            (editor-set-focused! (pi-app-editor app) #f)
            (%pi-app-active-selector-set! app
              (make-session-selector current
                all
                (lambda (path) (%confirm-resume! app path))
                (lambda () (pi-app-close-selector! app))
              ) ;make-session-selector
            ) ;%pi-app-active-selector-set!
            (pi-app-render! app)
          ) ;let-values
        ) ;lambda
        (lambda (tag . args) (%refuse! app "Failed to load resume sessions"))
      ) ;catch
    ) ;define

    ;; 确认选择：关闭选择器并恢复编辑器，向 jobs-chan 投递切换任务；
    ;; 实际的 sfile-box 切换与名称回填由后台 worker 完成后经事件上屏
    (define (%confirm-resume! app path)
      (%pi-app-active-selector-set! app #f)
      (editor-set-focused! (pi-app-editor app) #t)
      (if (%busy? app)
        ;; 选择器打开期间 agent 可能转为忙碌：此时切换会让在途消息写入旧会话
        (%refuse! app %busy-message)
        (catch #t
          (lambda () (chan-send! (pi-app-jobs-chan app) (list 'session_resume path) 1000))
          (lambda (tag . args)
            (container-add! (%pi-app-chat app)
              (make-tool-card "resume" '() 'error "Failed to resume session")
            ) ;container-add!
          ) ;lambda
        ) ;catch
      ) ;if
      (pi-app-render! app)
    ) ;define

    ;; 忙碌判定：agent 正在处理消息，或仍有排队消息等待结算，
    ;; 此时切换会话会让后续消息写入未知的会话文件，故拒绝切换
    (define (%busy? app)
      (let ((q (pi-app-queue app)))
        (or (pi-queue-busy? q) (not (pi-queue-empty? q)))
      ) ;let
    ) ;define

    (define (pi-app-submit-resume! app text)
      (cond
       ((not (string=? (string-trim-both text) "/resume"))
        (%refuse! app "Usage: /resume")
       ) ;
       ((%busy? app) (%refuse! app %busy-message))
       ((pi-app-no-session? app)
        (%refuse! app "Session persistence is disabled (--no-session)")
       ) ;
       ((not (procedure? (pi-app-resume-sessions app)))
        (%refuse! app "Session listing is unavailable")
       ) ;
       (else (%show-resume-selector! app))
      ) ;cond
    ) ;define

    (define (%resume-ref alist key)
      (cdr (or (assoc key alist) (cons #f #f)))
    ) ;define

    ;; 目标模型按 provider + id 在可用模型列表中精确匹配
    (define (%resume-find-model app provider model-id)
      (find
        (lambda (model)
          (and (equal? (%resume-ref model 'provider) provider)
            (equal? (%resume-ref model 'id) model-id)
          ) ;and
        ) ;lambda
        (or (pi-app-models app) '())
      ) ;find
    ) ;define

    ;; 应用目标会话记录的思考等级：仅模型支持且值合法时生效；
    ;; 不支持或值非法时收敛为 off 并返回提示文案（#f 表示无需提示），
    ;; 状态未记录思考等级时保留模型默认等级
    (define (%apply-resume-thinking! app state)
      (let ((level (%resume-ref state 'thinkingLevel)))
        (cond ((not (string? level)) #f)
              ((not (pi-app-supports-thinking? app))
               (pi-app-apply-thinking-level-ui! app "off")
               (string-append "thinking level \"" level "\" unsupported; using \"off\"")
              ) ;
              ((member level pi-thinking-levels)
               (pi-app-apply-thinking-level-ui! app level)
               #f
              ) ;
              (else (pi-app-apply-thinking-level-ui! app "off")
                (string-append "unknown thinking level \"" level "\"; using \"off\"")
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;; 应用目标会话的投影状态，返回 (values applied? note label)：
    ;; - state 缺失或未记录模型：保留当前界面值，applied? = #t
    ;; - 模型不在可用列表：保留当前值，applied? = #f，note 为警告文案
    ;; - 模型匹配：经 /model 的界面路径更新（不持久化默认模型），label 为
    ;;   实际生效的 provider/id，思考等级按状态收敛并在 note 中提示
    (define (%apply-resume-state! app state)
      (if (not (pair? state))
        (values #t #f #f)
        (let* ((state-model (%resume-ref state 'model))
               (provider (and (pair? state-model) (%resume-ref state-model 'provider)))
               ;; 1201-09 用 modelId；兼容 TUI 状态条目写入的 model-id
               (model-id (and (pair? state-model)
                           (or (%resume-ref state-model 'modelId) (%resume-ref state-model 'model-id))
                         ) ;and
               ) ;model-id
              ) ;
          (if (not (and (string? provider) (string? model-id)))
            (values #t #f #f)
            (let ((model (%resume-find-model app provider model-id)))
              (if (not model)
                (values #f
                  (string-append "model " provider "/" model-id
                    " is unavailable; keeping current model"
                  ) ;string-append
                  #f
                ) ;values
                (begin
                  (pi-app-apply-model-ui! app model)
                  (values #t
                    (%apply-resume-thinking! app state)
                    (string-append provider "/" model-id)
                  ) ;values
                ) ;begin
              ) ;if
            ) ;let
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    ;; worker 完成切换（session_resumed path name state）/ 失败（session_resume_error）
    (define (pi-app-handle-resume-event! app ev)
      (case (car ev)
            ((session_resumed)
             (let* ((path (cadr ev))
                    (name (and (> (length ev) 2) (caddr ev)))
                    (state (and (> (length ev) 3) (list-ref ev 3)))
                    (base (if name
                            (string-append "Resumed session: " name)
                            (string-append "Resumed session: " path)
                          ) ;if
                    ) ;base
                   ) ;
               ;; 未命名会话也要清空旧名称，避免 footer 显示陈旧会话名
               (%pi-app-session-name-set! app name)
               (pi-footer-set-session-name! (%pi-app-footer app) name)
               (let-values (((applied? note label) (%apply-resume-state! app state)))
                 ;; 提示卡片只保留一张：以会话名为主，匹配成功时附上实际模型
                 (if applied?
                   (container-add! (%pi-app-chat app)
                     (make-assistant-card (string-append base
                                            (if label (string-append " (" label ")") "")
                                            (if note (string-append " (" note ")") "")
                                          ) ;string-append
                     ) ;make-assistant-card
                   ) ;container-add!
                   (container-add! (%pi-app-chat app)
                     (make-tool-card "resume" '() 'error (string-append base " (" note ")"))
                   ) ;container-add!
                 ) ;if
               ) ;let-values
               (pi-app-render! app)
             ) ;let*
            ) ;
            ((session_resume_error)
             (container-add! (%pi-app-chat app)
               (make-tool-card "resume" '() 'error (cadr ev))
             ) ;container-add!
             (pi-app-render! app)
            ) ;
            (else #f)
      ) ;case
    ) ;define

  ) ;begin
) ;define-library
