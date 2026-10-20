(import (liii check)
  (liii string)
  (liii tui terminal)
)

(check-set-mode! 'report-failed)

;; terminal-begin-sync! 终端同步输出开始测试

;; 1. virtual-terminal 捕获同步开始序列
(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (terminal-begin-sync! vt)
  (check (virtual-terminal-raw vt) => '("\x1b[?2026h"))
)

;; 2. standard-terminal 输出 DECSET 2026 开启序列
(let* ((p (open-output-string))
       (st (make-standard-terminal :output-port p))
      )
  (terminal-begin-sync! st)
  (check (string-contains? (get-output-string p) "\x1b;[?2026h") => #t)
)

;; 3. 类型错误检查
(check-catch 'type-error (terminal-begin-sync! "not-a-terminal"))
(check-catch 'type-error (terminal-begin-sync! 12345))

(check-report)
