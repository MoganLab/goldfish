(import (liii check) (liii os))

(check-set-mode! 'report-failed)

;; 1. 参数类型检查
(check-catch 'type-error (console-raw-mode! "invalid"))
(check-catch 'type-error (console-raw-mode! 123))
(check-catch 'type-error (console-raw-mode! 'symbol))
(check-catch 'type-error (console-raw-mode! '()))

;; 2. console-raw-mode! 调用返回布尔值
(check-true (boolean? (console-raw-mode! #f)))

;; 3. console-kbhit? 返回布尔值（支持无参和可选毫秒超时参数）
(check-true (boolean? (console-kbhit?)))
(check-true (boolean? (console-kbhit? 0)))
(check-true (boolean? (console-kbhit? 10)))
(check-catch 'type-error (console-kbhit? "invalid"))
(check-catch 'type-error (console-kbhit? 1.5))

;; 4. console-getch 在无输入或非控制台时安全返回（整数或 #f）
(let ((c (console-getch)))
  (check-true (or (boolean? c) (exact-integer? c)))
) ;let

;; 5. raw mode 必须关闭 ISIG：否则 tty 会把 Ctrl+C 转成 SIGINT 直接杀死进程，
;;    TUI 永远收不到 0x03 字节，'ctrl-c 分支成为死代码（历史回归点）。
;;    ISIG 仅 POSIX 存在；Windows 走 ENABLE_PROCESSED_INPUT，由 C 层统一返回 #t。
(let ((state (console-raw-mode-state)))
  ;; 返回值必须是合法 alist
  (check-true (list? state))
  (check-true (pair? (assoc 'isig-disabled state)))
  (check-true (pair? (assoc 'icanon-disabled state)))
  (check-true (pair? (assoc 'echo-disabled state)))
  (check-true (pair? (assoc 'ixon-disabled state)))
  (check-true (pair? (assoc 'raw-active state)))
  ;; 未进入 raw mode 时不断言具体值（可能是任意终端状态），
  ;; 仅验证数据结构；真实断言见下方开启 raw mode 后的检查
) ;let

;; 6. 开启 raw mode 后，ISIG/ICANON/ECHO/IXON 必须全部关闭
(let* ((prev (console-raw-mode! #t))
       (state (console-raw-mode-state))
      ) ;
  (check-true (boolean? prev))
  ;; 仅在确实进入 raw mode 时断言（非 tty 环境如 CI 管道下 raw-active 为 #f）
  (when (eq? (cdr (assoc 'raw-active state)) #t)
    (check (cdr (assoc 'isig-disabled state)) => #t)
    (check (cdr (assoc 'icanon-disabled state)) => #t)
    (check (cdr (assoc 'echo-disabled state)) => #t)
    (check (cdr (assoc 'ixon-disabled state)) => #t)
  ) ;when
  ;; 恢复原终端状态，避免影响后续测试
  (console-raw-mode! #f)
) ;let*

(check-report)
