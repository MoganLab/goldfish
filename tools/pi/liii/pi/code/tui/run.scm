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
                (cond
                  ;; Ctrl+C：仅清空输入框，从不退出、也不中断正在跑的任务。
                  ((eq? k 'ctrl-c)
                   (unless (string-null? (editor-text ed))
                     (editor-set-text! ed "")
                   ) ;unless
                   (pi-app-render! app)
                   (loop)
                  ) ;
                  ;; Ctrl+D：仅在输入框为空时退出（EOF 语义），有内容时无操作
                  ((eq? k 'ctrl-d) (if (string-null? (editor-text ed)) #t (loop)))
                  ;; 按 Escape 时：若队列有等待中的消息，撤回至 editor 重新编辑
                  ((eq? k 'escape)
                   (let ((restored (pi-queue-restore-all! q)))
                     (if (pair? restored)
                       (begin
                         (editor-set-text! ed (string-join restored "\n"))
                         (pi-app-render! app)
                         (loop)
                       ) ;begin
                       #t
                     ) ;if
                   ) ;let
                  ) ;
                  ((not k) (sleep 0.01) (loop))
                  (else (component-handle-input ed k) (pi-app-render! app) (loop))
                ) ;cond
              ) ;let
            ) ;let
          ) ;lambda
          (lambda ()
            ;; 退出清理：光标必须跨过整个 TUI 块，否则 shell prompt 会画在输入框位置
            (catch #t
              (lambda ()
                (let* ((lines (screen-lines screen))
                       (total (length lines))
                       ;; inline 模式下 total 只是内容行数，而光标实际停在输入行；
                       ;; 取渲染行数与曾到达的最大行数的较大者才能真正落到块下方
                       (max-y (terminal-max-y term))
                       (bottom (max total max-y))
                      ) ;
                  (when (> total 0)
                    (terminal-set-cursor! term 0 bottom)
                  ) ;when
                ) ;let*
              ) ;lambda
              (lambda (tag . args) #f)
            ) ;catch
            (terminal-show-cursor! term)
            (terminal-exit-raw-mode! term)
            (terminal-write term "\r\n")
            (terminal-flush term)
          ) ;lambda
        ) ;dynamic-wind
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
