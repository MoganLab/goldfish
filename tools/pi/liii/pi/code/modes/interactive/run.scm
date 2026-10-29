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

    ;; 退出备用屏后把对话记录打印回主屏，
    ;; 对齐上游 fullscreenExitOutput=transcript 的行为。
    ;; 仅标准终端执行：虚拟终端（测试用）没有真实输出端口。
    (define (%pi-print-transcript app)
      (let ((term (pi-app-term app)))
        (when (standard-terminal? term)
          (catch #t
            (lambda ()
              (let* ((w (car (terminal-size term))) (lines (component-render (pi-app-chat app) w)))
                (for-each (lambda (line) (terminal-write term line) (terminal-write term "\r\n"))
                  lines
                ) ;for-each
                (terminal-flush term)
              ) ;let*
            ) ;lambda
            (lambda args #f)
          ) ;catch
        ) ;when
      ) ;let
    ) ;define

    (define* (pi-app-run app (initial-msg "") (rest-msgs '()))
      (let* ((term (pi-app-term app))
             (ed (pi-app-editor app))
             (q (pi-app-queue app))
             ;; 分支切换监听：worker 会话回调经 event-chan 发 branch_changed，
             ;; 由主循环 pi-app-poll-events! 消费并更新 footer；
             ;; 非 git 仓库时为 #f（make-branch-watcher 内部优雅降级）
             (watcher (make-branch-watcher (pi-app-event-chan app) (detect-git-dir)))
            ) ;
        (dynamic-wind (lambda ()
                        (terminal-enter-raw-mode! term)
                        ;; 开启 bracketed paste：粘贴内容整体交给编辑器，
                        ;; 其中的换行不会被当成 Enter 提交
                        (terminal-set-paste-mode! #t)
                        (terminal-write term "\x1b;[?2004h")
                        ;; 进入备用屏：整屏由 TUI 独占，退出后主屏内容原样恢复
                        (terminal-enter-alt-screen! term)
                        ;; 开启鼠标事件上报（XTerm 1000/1002/1006）：
                        ;; 滚轮用于滚动对话记录
                        (terminal-mouse-mode! term #t)
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
                  ;; Ctrl+D：输入框严格为空时退出（EOF 语义）；
                  ;; 非空时不再吞掉按键，落到下方通用分支交给编辑器
                  ;; 前向删除光标右侧字符（与 Delete 键一致）。
                  ;; 同一次按键只执行一种语义：删除后恰好变空也不退出，
                  ;; 下一次独立 Ctrl+D 才按 EOF 语义退出。
                  ((and (not (pi-app-active-selector app))
                     (eq? k 'ctrl-d)
                     (string-null? (editor-text ed))
                   ) ;and
                   #t
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
                  ;; 对话记录滚动：page-up/page-down 整页，
                  ;; ctrl+shift+up/down 单行，ctrl+shift+home/end 跳到首尾
                  ((and (not (pi-app-active-selector app)) (eq? k 'page-up))
                   (pi-app-scroll-up! app)
                   (pi-app-render! app)
                   (loop)
                  ) ;
                  ((and (not (pi-app-active-selector app)) (eq? k 'page-down))
                   (pi-app-scroll-down! app)
                   (pi-app-render! app)
                   (loop)
                  ) ;
                  ((and (not (pi-app-active-selector app)) (eq? k 'shift-ctrl-up))
                   (pi-app-scroll-line-up! app)
                   (pi-app-render! app)
                   (loop)
                  ) ;
                  ((and (not (pi-app-active-selector app)) (eq? k 'shift-ctrl-down))
                   (pi-app-scroll-line-down! app)
                   (pi-app-render! app)
                   (loop)
                  ) ;
                  ((and (not (pi-app-active-selector app)) (eq? k 'shift-ctrl-home))
                   (pi-app-scroll-top! app)
                   (pi-app-render! app)
                   (loop)
                  ) ;
                  ((and (not (pi-app-active-selector app)) (eq? k 'shift-ctrl-end))
                   (pi-app-scroll-bottom! app)
                   (pi-app-render! app)
                   (loop)
                  ) ;
                  ;; 鼠标事件：滚轮上下滚动对话记录
                  ((and (mouse-event? k) (not (pi-app-active-selector app)))
                   (when (pi-app-handle-mouse! app k)
                     (pi-app-render! app)
                   ) ;when
                   (loop)
                  ) ;
                  ;; 按 ctrl+o 时：全局切换 bash/工具输出卡片的展开与折叠
                  ((eq? k 'ctrl-o) (pi-app-toggle-tool-output! app) (pi-app-render! app) (loop))
                  ((not k)
                   ;; 空闲轮询：busy 时推进 Working spinner 动画（内部自带 80ms 节拍）。
                   ;; 自适应休眠：busy 保持 10ms 保证流式上屏与动画顺滑；
                   ;; 空闲时只等用户按键，睡 50ms（键盘延迟无感，
                   ;; 每秒唤醒从 100 次降到 20 次，显著降低空闲 CPU 底噪）
                   (pi-app-working-tick! app)
                   (sleep (if (pi-queue-busy? q) 0.01 0.05))
                   (loop)
                  ) ;
                  (else (pi-app-handle-input! app k) (pi-app-render! app) (loop))
                ) ;cond
              ) ;let
            ) ;let
          ) ;lambda
          (lambda ()
            ;; 退出清理：先关闭鼠标事件上报（与进入时对称），
            ;; 再离开备用屏（主屏内容自动恢复），
            ;; 最后把对话记录打印回主屏，避免退出后看不到本次会话内容
            (catch #t (lambda () (terminal-mouse-mode! term #f)) (lambda (tag . args) #f))
            (catch #t (lambda () (terminal-exit-alt-screen! term)) (lambda (tag . args) #f))
            (%pi-print-transcript app)
            (terminal-write term "\x1b;[?2004l")
            (terminal-set-paste-mode! #f)
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
