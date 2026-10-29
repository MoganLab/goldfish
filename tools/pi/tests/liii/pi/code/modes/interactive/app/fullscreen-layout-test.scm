(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive run)
) ;import

(check-set-mode! 'report-failed)

;; 全屏（备用屏）模式布局：
;;   [对话记录视口]  可滚动，follow 末尾
;;   [输入区]
;;   [页脚]
;; 对齐上游 packages/coding-agent/src/modes/interactive/chat-viewport.ts

;; 1. 帧高度固定为终端高度，输入区与页脚贴在屏幕底部
(let* ((vterm (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t)))
       (screen (pi-app-screen app))
      ) ;
  (pi-app-render! app)
  (let ((plain (map strip-ansi (alt-screen-lines screen))))
    ;; 24 行终端：对话记录视口 19 行 + 输入区 3 行 + 页脚 2 行
    (check (length plain) => 24)
    ;; 页眉位于对话记录视口顶部
    (check-true (string-contains? (list-ref plain 1) " gf v"))
    ;; 输入区上边框贴在底部（视口 19 + 上边框）
    (check-true (string-contains? (list-ref plain 19) "─"))
    ;; 页脚最后两行
    (check-true (string-contains? (list-ref plain 22) "("))
  ) ;let
) ;let*

;; 2. 对话记录超出视口时视口跟随末尾，滚动后可回看历史
(let* ((vterm (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t)))
       (screen (pi-app-screen app))
      ) ;
  ;; 每个用户卡片占 4 行，10 条消息共 40 行，超出 19 行视口
  (let loop
    ((i 0))
    (when (< i 10)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (pi-app-render! app)
  (let ((plain (map strip-ansi (alt-screen-lines screen))))
    ;; 视口跟随末尾：最后一条消息可见
    (check-true (string-contains? (string-join plain "\n") "msg-9"))
    ;; 首条消息已滚出视口
    (check-false (string-contains? (string-join plain "\n") "msg-0"))
  ) ;let
  ;; 向上翻页：可以看到更早的消息，最新的消息滚出视口
  (pi-app-scroll-up! app)
  (pi-app-render! app)
  (let ((plain (map strip-ansi (alt-screen-lines screen))))
    (check-true (string-contains? (string-join plain "\n") "msg-1"))
    (check-false (string-contains? (string-join plain "\n") "msg-9"))
  ) ;let
  ;; 回到末尾：恢复 follow
  (pi-app-scroll-bottom! app)
  (pi-app-render! app)
  (let ((plain (map strip-ansi (alt-screen-lines screen))))
    (check-true (string-contains? (string-join plain "\n") "msg-9"))
  ) ;let
  ;; 跳到顶部：首条消息重新可见
  (pi-app-scroll-top! app)
  (pi-app-render! app)
  (let ((plain (map strip-ansi (alt-screen-lines screen))))
    (check-true (string-contains? (string-join plain "\n") "msg-0"))
    (check-false (string-contains? (string-join plain "\n") "msg-9"))
  ) ;let
) ;let*

;; 3. 滚动不影响输入区位置（dock 始终贴在屏幕底部）
(let* ((vterm (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t)))
       (screen (pi-app-screen app))
      ) ;
  (pi-app-add-user-message! app "hello")
  (pi-app-render! app)
  (let ((before (terminal-cursor vterm)))
    (pi-app-scroll-up! app)
    (pi-app-render! app)
    (check (terminal-cursor vterm) => before)
  ) ;let
) ;let*

;; 4. 主循环按键接线：page-up 滚动对话记录
(let* ((vterm (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t)))
       (screen (pi-app-screen app))
      ) ;
  (let loop
    ((i 0))
    (when (< i 10)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (virtual-terminal-push-key! vterm 'page-up)
  (virtual-terminal-push-key! vterm 'ctrl-d)
  (check (pi-app-run app) => #t)
  (let ((plain (map strip-ansi (alt-screen-lines screen))))
    (check-true (string-contains? (string-join plain "\n") "msg-1"))
    (check-false (string-contains? (string-join plain "\n") "msg-9"))
  ) ;let
) ;let*

;; 5. 发送新消息时自动拉回末尾（恢复 follow）
(let* ((vterm (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t)))
       (screen (pi-app-screen app))
      ) ;
  (let loop
    ((i 0))
    (when (< i 10)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (pi-app-render! app)
  ;; 向上翻页，离开跟随位置
  (pi-app-scroll-up! app)
  (pi-app-render! app)
  (check-false (scroll-view-follow? (pi-app-scroll-view app)))
  ;; 发送新消息：视口回到末尾
  (pi-app-submit! app "new-message")
  (pi-app-render! app)
  (check (scroll-view-follow? (pi-app-scroll-view app)) => #t)
  (let ((plain (map strip-ansi (alt-screen-lines screen))))
    (check-true (string-contains? (string-join plain "\n") "new-message"))
  ) ;let
) ;let*

;; 6. 鼠标滚轮：向上滚动对话记录
(let* ((vterm (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t)))
       (screen (pi-app-screen app))
       (sv (pi-app-scroll-view app))
      ) ;
  (let loop
    ((i 0))
    (when (< i 10)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (pi-app-render! app)
  ;; 初始跟随末尾
  (check (scroll-view-follow? sv) => #t)
  (let ((before (scroll-view-offset sv)))
    ;; 滚轮向上：视口离开跟随位置，向上滚动
    (check (pi-app-handle-mouse! app (make-mouse-event 'wheel-up 'none 10 5)) => #t)
    (pi-app-render! app)
    (check (scroll-view-follow? sv) => #f)
    (check (< (scroll-view-offset sv) before) => #t)
  ) ;let
  ;; 反复向下滚动回到末尾，恢复跟随
  (let loop
    ((i 0))
    (when (< i 40)
      (pi-app-handle-mouse! app (make-mouse-event 'wheel-down 'none 10 5))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (pi-app-render! app)
  (check (scroll-view-follow? sv) => #t)
  ;; 非滚轮事件（按下/释放/拖动）暂不处理
  (check (pi-app-handle-mouse! app (make-mouse-event 'press 'left 10 5)) => #f)
) ;let*

;; 7. 主循环接线：滚轮事件经 virtual terminal 注入后滚动对话记录
(let* ((vterm (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t)))
       (screen (pi-app-screen app))
       (sv (pi-app-scroll-view app))
      ) ;
  (let loop
    ((i 0))
    (when (< i 10)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (virtual-terminal-push-key! vterm (make-mouse-event 'wheel-up 'none 10 5))
  (virtual-terminal-push-key! vterm 'ctrl-d)
  (check (pi-app-run app) => #t)
  ;; 滚轮事件已被主循环消费：视口离开跟随位置
  (check (scroll-view-follow? sv) => #f)
  ;; 进入/退出 TUI 时开启并关闭鼠标事件上报
  (let ((raw (string-join (virtual-terminal-raw vterm) "")))
    (check-true (string-contains? raw "\x1b;[?1000h"))
    (check-true (string-contains? raw "\x1b;[?1000l"))
  ) ;let
) ;let*

;; 8. 离开跟随位置时，视口底部显示“跳到最新消息”提示（Ctrl+End）
(let* ((vterm (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t)))
       (screen (pi-app-screen app))
      ) ;
  (let loop
    ((i 0))
    (when (< i 10)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (define (frame-text)
    (string-join (map strip-ansi (alt-screen-lines screen)) "\n")
  ) ;define
  ;; 跟随末尾时没有提示
  (pi-app-render! app)
  (check-false (string-contains? (frame-text) "Jump to latest message"))
  ;; 向上滚动后出现提示，并指明快捷键
  (pi-app-scroll-up! app)
  (pi-app-render! app)
  (check-true (string-contains? (frame-text) "Jump to latest message"))
  (check-true (string-contains? (frame-text) "ctrl+end"))
  ;; 回到末尾后提示消失
  (pi-app-scroll-bottom! app)
  (pi-app-render! app)
  (check-false (string-contains? (frame-text) "Jump to latest message"))
) ;let*

;; 9. Ctrl+End 跳到最下方（恢复跟随）
(let* ((vterm (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t)))
       (screen (pi-app-screen app))
       (sv (pi-app-scroll-view app))
      ) ;
  (let loop
    ((i 0))
    (when (< i 10)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  ;; 先向上翻页离开跟随，再按 Ctrl+End 回到最下方
  (virtual-terminal-push-key! vterm 'page-up)
  (virtual-terminal-push-key! vterm 'ctrl-end)
  (virtual-terminal-push-key! vterm 'ctrl-d)
  (check (pi-app-run app) => #t)
  (check (scroll-view-follow? sv) => #t)
  (let ((plain (map strip-ansi (alt-screen-lines screen))))
    (check-true (string-contains? (string-join plain "\n") "msg-9"))
    (check-false (string-contains? (string-join plain "\n") "Jump to latest message"))
  ) ;let
) ;let*

(check-report)
