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
    (liii pi code core session-manager)
    (liii pi code core session-list)
    (liii pi code modes interactive session-selector)
  ) ;import
  (export pi-run-session-selector pi-load-resume-sessions)
  (begin

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
             (screen (make-main-screen term :auto-hide-cursor #f))
             (selected #f)
             (done? #f)
             (selector (make-session-selector current-sessions
                         all-sessions
                         (lambda (path) (set! selected path) (set! done? #t))
                         (lambda () (set! done? #t))
                       ) ;make-session-selector
             ) ;selector
             (render!
               (lambda ()
                 (screen-render! screen
                   (component-render selector (car (terminal-size term)))
                   '(2 . 6)
                 ) ;screen-render!
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
                  (if key (begin (component-handle-input selector key) (render!)) (sleep 0.02))
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
