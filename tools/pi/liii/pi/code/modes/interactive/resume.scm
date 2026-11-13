;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; --resume 的轻量启动 TUI：返回所选文件路径，取消时返回 #f。
(define-library (liii pi code modes interactive resume)
  (import (scheme base)
    (liii time)
    (liii tui)
    (liii pi code cli args)
    (liii pi code core session-manager)
    (liii pi code core session-list)
    (liii pi code modes interactive session-selector)
  ) ;import
  (export pi-run-session-selector pi-load-resume-sessions pi-resolve-history-file)
  (begin

    ;; 启动恢复用的会话文件：显式 --session 优先，其次 --continue 取目录最新会话；
    ;; 与 worker 的 setup-session-file 共用同一解析入口，保证前台可见历史与
    ;; 后台落盘目标同源；--no-session 时恒为 #f（不读取也不伪造历史）
    (define (pi-resolve-history-file parsed cfg cwd)
      (if (pi-args-ref parsed 'no-session)
        #f
        (or (pi-args-ref parsed 'session-file)
          (and (pi-args-ref parsed 'continue)
            (catch #t
              (lambda ()
                (resolve-resume-session-file parsed (resolve-session-dir parsed cfg cwd))
              ) ;lambda
              (lambda args #f)
            ) ;catch
          ) ;and
        ) ;or
      ) ;if
    ) ;define

    (define (pi-load-resume-sessions parsed cfg cwd)
      (let* ((current-dir (resolve-session-dir parsed cfg cwd))
             (agent-dir (cdr (assoc 'agent-dir cfg)))
             (default-layout? (string=? current-dir (get-session-dir agent-dir cwd)))
             (current (pi-list-session-infos current-dir))
            ) ;
        (values current
          (lambda ()
            (if default-layout?
              (pi-list-session-infos (get-sessions-root agent-dir) #t)
              current
            ) ;if
          ) ;lambda
        ) ;values
      ) ;let*
    ) ;define

    (define* (pi-run-session-selector current-sessions all-sessions (terminal #f))
      (let* ((term (or terminal (make-standard-terminal :inline #t)))
             ;; 软件方块光标由搜索框反显绘制，硬件光标保持隐藏
             (screen (make-main-screen term :auto-hide-cursor #t))
             (selected #f)
             (done? #f)
             (selector (make-session-selector current-sessions
                         all-sessions
                         (lambda (path) (set! selected path) (set! done? #t))
                         (lambda () (set! done? #t))
                         :incremental #t
                       ) ;make-session-selector
             ) ;selector
             (render!
               (lambda ()
                 ;; 不传硬件光标位置：auto-hide 保持隐藏，
                 ;; 软件方块光标由搜索框反显绘制
                 (screen-render! screen (component-render selector (car (terminal-size term))))
               ) ;lambda
             ) ;render!
            ) ;
        (dynamic-wind (lambda ()
                        (terminal-enter-raw-mode! term)
                        (terminal-hide-cursor! term)
                        (render!)
                      ) ;lambda
          (lambda ()
            (let loop
              ()
              (unless done?
                (let ((key (terminal-poll-key term)))
                  (if key
                    (begin
                      (component-handle-input selector key)
                      (render!)
                    ) ;begin
                    (let ((step (session-selector-step-index! selector)))
                      (when (eq? step 'updated)
                        (render!)
                      ) ;when
                      (unless step
                        (sleep 0.02)
                      ) ;unless
                    ) ;let
                  ) ;if
                  (loop)
                ) ;let
              ) ;unless
            ) ;let
          ) ;lambda
          (lambda ()
            (terminal-set-cursor! term
              0
              (max (length (screen-lines screen)) (terminal-max-y term))
            ) ;terminal-set-cursor!
            (terminal-show-cursor! term)
            (terminal-exit-raw-mode! term)
            (terminal-write term "\r\n")
            (terminal-flush term)
          ) ;lambda
        ) ;dynamic-wind
        selected
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
