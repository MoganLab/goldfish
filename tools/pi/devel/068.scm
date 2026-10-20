(import (scheme base)
  (liii check)
  (liii os)
  (liii tui terminal)
  (liii tui terminal poll)
) ;import

(check-set-mode! 'report-failed)

;; ======================================================================
;; 任务 068: Linux 平台终端原始模式与按键适配 自动化验收脚本
;; ======================================================================

(display "=== 验收点 1: POSIX 终端 Raw 模式与类型安全 ===\n")
;; 1.1 参数校验
(check-catch 'type-error (console-raw-mode! "invalid"))
(check-catch 'type-error (console-raw-mode! 123))
(check-catch 'type-error (console-raw-mode! 'symbol))
(check-catch 'type-error (console-raw-mode! '()))

;; 1.2 开关调用安全返回布尔值
(check-true (boolean? (console-raw-mode! #f)))

(display "=== 验收点 2: 非阻塞按键探测与超时机制 ===\n")
;; 2.1 默认完全非阻塞 (0ms)
(check-true (boolean? (console-kbhit?)))
(check-true (boolean? (console-kbhit? 0)))

;; 2.2 带超时参数调用
(check-true (boolean? (console-kbhit? 5)))
(check-true (boolean? (console-kbhit? 10)))

;; 2.3 超时参数类型防御
(check-catch 'type-error (console-kbhit? "invalid"))
(check-catch 'type-error (console-kbhit? 1.5))
(check-catch 'type-error (console-kbhit? #t))

(display "=== 验收点 3: 非阻塞字符读取安全性 ===\n")
;; 3.1 console-getch 在无输入或非控制台时安全返回（整数或 #f）
(let ((c (console-getch)))
  (check-true (or (boolean? c) (exact-integer? c)))
) ;let

(display "=== 验收点 4: ANSI CSI 与 SS3 序列纯函数解析 ===\n")
;; 4.1 独立 ESC 键
(check (%resolve-ansi-bytes '(27)) => 'escape)

;; 4.2 CSI 方向键 (ESC [ A/B/C/D)
(check (%resolve-ansi-bytes '(27 91 65)) => 'up)
(check (%resolve-ansi-bytes '(27 91 66)) => 'down)
(check (%resolve-ansi-bytes '(27 91 67)) => 'right)
(check (%resolve-ansi-bytes '(27 91 68)) => 'left)

;; 4.3 CSI 导航与编辑键 (Home, End, Delete, PageUp, PageDown)
(check (%resolve-ansi-bytes '(27 91 72)) => 'home)
(check (%resolve-ansi-bytes '(27 91 70)) => 'end)
(check (%resolve-ansi-bytes '(27 91 51 126)) => 'delete)
(check (%resolve-ansi-bytes '(27 91 53 126)) => 'page-up)
(check (%resolve-ansi-bytes '(27 91 54 126)) => 'page-down)

;; 4.4 SS3 格式方向键与功能键 (ESC O A/B/C/D/H/F/P/Q)
(check (%resolve-ansi-bytes '(27 79 65)) => 'up)
(check (%resolve-ansi-bytes '(27 79 66)) => 'down)
(check (%resolve-ansi-bytes '(27 79 67)) => 'right)
(check (%resolve-ansi-bytes '(27 79 68)) => 'left)
(check (%resolve-ansi-bytes '(27 79 72)) => 'home)
(check (%resolve-ansi-bytes '(27 79 70)) => 'end)
(check (%resolve-ansi-bytes '(27 79 80)) => 'f1)
(check (%resolve-ansi-bytes '(27 79 81)) => 'f2)

;; 4.5 空列表防御
(check (%resolve-ansi-bytes '()) => #f)

(display "=== 验收点 5: Terminal 按键注入与事件轮询 ===\n")
(let ((vt (make-virtual-terminal 40 20)))
  ;; 初始状态无输入
  (check (terminal-poll-key vt) => #f)

  ;; 注入按键后顺序弹出
  (virtual-terminal-push-key! vt 'up)
  (virtual-terminal-push-key! vt 'down)
  (virtual-terminal-push-key! vt "a")
  (virtual-terminal-push-key! vt 'enter)
  (virtual-terminal-push-key! vt 'escape)

  (check (terminal-poll-key vt) => 'up)
  (check (terminal-poll-key vt) => 'down)
  (check (terminal-poll-key vt) => "a")
  (check (terminal-poll-key vt) => 'enter)
  (check (terminal-poll-key vt) => 'escape)
  (check (terminal-poll-key vt) => #f)
) ;let

;; terminal-poll-key 参数类型校验
(check-catch 'type-error (terminal-poll-key 999))
(check-catch 'type-error (terminal-poll-key "not-a-terminal"))

(check-report)
