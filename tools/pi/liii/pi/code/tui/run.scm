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
    (liii pi code tui common)
  ) ;import
  (begin

    (define (pi-app-run app)
      (let* ((term (pi-app-term app))
             (screen (pi-app-screen app))
             (ed (pi-app-editor app))
             (q (pi-app-queue app))
             ;; 分支切换监听：worker 会话回调经 event-chan 发 branch_changed，
             ;; 由主循环 pi-app-poll-events! 消费并更新 footer；
             ;; 非 git 仓库时为 #f（make-branch-watcher 内部优雅降级）
             (watcher (make-branch-watcher (pi-app-event-chan app) (detect-git-dir)))
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
                      ;; 按 shift+tab 时：循环切换 thinking level，
                      ;; footer 右下角标签与编辑器边框/Working 颜色同步更新
                      ((eq? k 'shift-tab) (pi-app-cycle-thinking! app) (pi-app-render! app) (loop))
                      ;; 按 ctrl+o 时：全局切换 bash/工具输出卡片的展开与折叠
                      ((eq? k 'ctrl-o) (pi-app-toggle-tool-output! app) (pi-app-render! app) (loop))
                      ((not k)
                       ;; 空闲轮询：busy 时推进 Working spinner 动画（内部自带 80ms 节拍）
                       (pi-app-working-tick! app)
                       (sleep 0.01)
                       (loop)
                      ) ;
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
              ;; 关闭 bash 任务通道，后台 worker 收到 eof 后退出，
              ;; 避免进程退出时后台线程仍在阻塞 recv 导致崩溃
              (catch #t
                (lambda () (chan-close! (pi-app-bash-jobs-chan app)))
                (lambda args #f)
              ) ;catch
              ;; 停止分支监听：C 层 watcher 撤销，后台泵收到 stopped 后自行退出
              (stop-branch-watcher! watcher)
            ) ;let*
          ) ;lambda
        ) ;dynamic-wind
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
