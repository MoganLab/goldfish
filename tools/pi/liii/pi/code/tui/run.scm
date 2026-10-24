;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code tui run)
  (export pi-app-run)
  (import (scheme base)
    (liii time)
    (liii string)
    (liii tui)
    (liii pi agent queue)
    (liii pi code tui app)
  ) ;import
  (begin

    (define (pi-app-run app)
      (let* ((term (pi-app-term app))
             (screen (pi-app-screen app))
             (ed (pi-app-editor app))
             (q (pi-app-queue app))
            ) ;
        (dynamic-wind (lambda ()
                        (terminal-enter-raw-mode! term)
                        (terminal-hide-cursor! term)
                        (pi-app-render! app)
                      ) ;lambda
          (lambda ()
            (let loop
              ()
              ;; 优先轮询来自后台 Worker 的事件流上屏
              (pi-app-poll-events! app)
              (let ((k (terminal-poll-key term)))
                (cond ((or (eq? k 'ctrl-c) (eq? k 'ctrl-d)) #t)
                      ;; 按 Escape 时：若队列有等待中的消息，撤回至 editor 重新编辑；
                      ;; 队列为空时仅忽略该按键，不退出 TUI
                      ((eq? k 'escape)
                       (let ((restored (pi-queue-restore-all! q)))
                         (if (pair? restored)
                           (begin
                             (editor-set-text! ed (string-join restored "\n"))
                             (pi-app-render! app)
                           ) ;begin
                         ) ;if
                         (loop)
                       ) ;let
                      ) ;
                      ((not k) (sleep 0.01) (loop))
                      (else (component-handle-input ed k) (pi-app-render! app) (loop))
                ) ;cond
              ) ;let
            ) ;let
          ) ;lambda
          (lambda ()
            (let* ((lines (screen-lines screen)) (total (length lines)))
              (when (> total 0)
                (terminal-set-cursor! term 0 total)
              ) ;when
              (terminal-show-cursor! term)
              (terminal-exit-raw-mode! term)
              (terminal-write term "\r\n")
              (terminal-flush term)
            ) ;let*
          ) ;lambda
        ) ;dynamic-wind
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
