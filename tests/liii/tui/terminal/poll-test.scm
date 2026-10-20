(import (liii check)
  (liii tui terminal)
  (liii tui terminal poll)
) ;import

(check-set-mode! 'report-failed)

;; 1. 测试 virtual-terminal 初始无输入时返回 #f
(let ((vt (make-virtual-terminal 40 20)))
  (check (terminal-poll-key vt) => #f)
) ;let

;; 2. 测试 virtual-terminal 注入按键后顺序弹出
(let ((vt (make-virtual-terminal 40 20)))
  (virtual-terminal-push-key! vt 'up)
  (virtual-terminal-push-key! vt 'down)
  (virtual-terminal-push-key! vt "w")
  (virtual-terminal-push-key! vt 'enter)

  (check (terminal-poll-key vt) => 'up)
  (check (terminal-poll-key vt) => 'down)
  (check (terminal-poll-key vt) => "w")
  (check (terminal-poll-key vt) => 'enter)
  (check (terminal-poll-key vt) => #f)
) ;let

;; 3. 测试非法参数类型错误
(check-catch 'type-error (terminal-poll-key 123))
(check-catch 'type-error (terminal-poll-key "invalid"))
(check-catch 'type-error (virtual-terminal-push-key! "invalid" 'up))
(check-catch 'type-error (virtual-terminal-pop-key! "invalid"))

;; 4. 测试无参数调用（当前无按键时返回 #f 或非阻塞）
(check (boolean? (terminal-poll-key)) => #t)

(check-report)
