;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes interactive run)
  (export pi-app-run)
  (import (scheme base)
    (liii go)
    (liii time)
    (liii string)
    (liii tui)
    (liii pi agent queue)
    (liii pi code modes interactive app)
    (liii pi code modes interactive common)
  ) ;import
  (begin

    (define* (pi-app-run app (initial-msg "") (rest-msgs '()))
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
            (pi-app-submit-initial! app initial-msg rest-msgs)
            (let loop
              ()
              ;; 优先轮询来自后台 Worker 的事件流上屏
              (pi-app-poll-events! app)
              (let ((k (terminal-poll-key term)))
                (cond
                  ;; Ctrl+C：仅清空输入框，从不退出、也不中断正在跑的任务。
                  ((and (not (pi-app-active-selector app)) (eq? k 'ctrl-c))
                   (unless (string-null? (editor-text ed))
                     (editor-set-text! ed "")
                   ) ;unless
                   (pi-app-render! app)
                   (loop)
                  ) ;
                  ;; Ctrl+D：仅在输入框为空时退出（EOF 语义），有内容时无操作
                  ((and (not (pi-app-active-selector app)) (eq? k 'ctrl-d))
                   (if (string-null? (editor-text ed)) #t (loop))
                  ) ;
                  ;; 按 Escape 时（对齐上游 pi 的 onEscape）：
                  ;; busy：发 abort 信号中断正在进行的任务（worker 检查点
                  ;;   探测后以 aborted 结算）；队列空闲：仅撤回至 editor，
                  ;;   两种情况下都把排队消息恢复到编辑器重新编辑，不退出 TUI
                  ((and (not (pi-app-active-selector app))
                     (not (pi-app-slash-command-menu app))
                     (eq? k 'escape)
                   ) ;and
                   (when (pi-queue-busy? q)
                     ;; 非阻塞发送：容量 1，丢弃旧信号也无所谓（语义同为中止）
                     (chan-send! (pi-app-abort-chan app) 'abort 0)
                   ) ;when
                   (let ((restored (pi-queue-restore-all! q)))
                     (when (pair? restored)
                       (editor-set-text! ed (string-join restored "\n"))
                     ) ;when
                   ) ;let
                   (pi-app-render! app)
                   (loop)
                  ) ;
                  ;; 按 shift+tab 时：循环切换 thinking level，
                  ;; footer 右下角标签与编辑器边框/Working 颜色同步更新
                  ((and (not (pi-app-active-selector app)) (eq? k 'shift-tab))
                   (pi-app-cycle-thinking! app)
                   (pi-app-render! app)
                   (loop)
                  ) ;
                  ;; 按 ctrl+o 时：全局切换 bash/工具输出卡片的展开与折叠
                  ((eq? k 'ctrl-o) (pi-app-toggle-tool-output! app) (pi-app-render! app) (loop))
                  ((not k)
                   ;; 空闲轮询：busy 时推进 Working spinner 动画（内部自带 80ms 节拍）
                   (pi-app-working-tick! app)
                   (sleep 0.01)
                   (loop)
                  ) ;
                  (else (pi-app-handle-input! app k) (pi-app-render! app) (loop))
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
            ;; 关闭 bash 任务通道，后台 worker 收到 eof 后退出，
            ;; 避免进程退出时后台线程仍在阻塞 recv 导致崩溃
            (catch #t
              (lambda () (chan-close! (pi-app-bash-jobs-chan app)))
              (lambda args #f)
            ) ;catch
            ;; 停止分支监听：C 层 watcher 撤销，后台泵收到 stopped 后自行退出
            (stop-branch-watcher! watcher)
          ) ;lambda
        ) ;dynamic-wind
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
