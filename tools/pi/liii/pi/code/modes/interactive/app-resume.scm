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
  (export pi-app-resume-command? pi-app-submit-resume!
    pi-app-handle-resume-event!
  ) ;export
  (import (scheme base)
    (liii go)
    (liii string)
    (liii tui)
    (liii pi agent queue)
    (liii pi code modes interactive state)
    (liii pi code modes interactive render)
    (liii pi code modes interactive views)
    (liii pi code modes interactive footer)
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
        (lambda (tag . args)
          (%refuse! app "Failed to load resume sessions")
        ) ;lambda
      ) ;catch
    ) ;define

    ;; 确认选择：关闭选择器并恢复编辑器，向 jobs-chan 投递切换任务；
    ;; 实际的 sfile-box 切换与名称回填由后台 worker 完成后经事件上屏
    (define (%confirm-resume! app path)
      (%pi-app-active-selector-set! app #f)
      (editor-set-focused! (pi-app-editor app) #t)
      (catch #t
        (lambda () (chan-send! (pi-app-jobs-chan app) (list 'session_resume path) 1000))
        (lambda (tag . args)
          (container-add! (%pi-app-chat app)
            (make-tool-card "resume" '() 'error "Failed to resume session")
          ) ;container-add!
        ) ;lambda
      ) ;catch
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
      (cond ((not (string=? (string-trim-both text) "/resume"))
             (%refuse! app "Usage: /resume")
             ) ;
            ((%busy? app)
             (%refuse! app "Agent is busy; try /resume again later")
             ) ;
            ((pi-app-no-session? app)
             (%refuse! app "Session persistence is disabled (--no-session)")
             ) ;
            ((not (procedure? (pi-app-resume-sessions app)))
             (%refuse! app "Session listing is unavailable")
             ) ;
            (else (%show-resume-selector! app))
      ) ;cond
    ) ;define

    ;; worker 完成切换（session_resumed path name）/ 失败（session_resume_error）
    (define (pi-app-handle-resume-event! app ev)
      (case (car ev)
            ((session_resumed)
             (let* ((path (cadr ev))
                    (name (and (> (length ev) 2) (caddr ev)))
                   ) ;
               ;; 未命名会话也要清空旧名称，避免 footer 显示陈旧会话名
               (%pi-app-session-name-set! app name)
               (pi-footer-set-session-name! (%pi-app-footer app) name)
               (container-add! (%pi-app-chat app)
                 (make-assistant-card
                   (if name
                     (string-append "Resumed session: " name)
                     (string-append "Resumed session: " path)
                   ) ;if
                 ) ;make-assistant-card
               ) ;container-add!
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
