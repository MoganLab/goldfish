(import (liii check)
  (liii string)
  (liii tui terminal)
)

(check-set-mode! 'report-failed)

;; terminal-end-sync! 终端同步输出结束测试

;; 1. virtual-terminal 捕获同步结束序列
(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (terminal-end-sync! vt)
  (check (virtual-terminal-raw vt) => '("\x1b[?2026l"))
)

;; 2. standard-terminal 输出 DECSET 2026 结束序列
(let* ((p (open-output-string))
       (st (make-standard-terminal :output-port p))
      )
  (terminal-end-sync! st)
  (check (string-contains? (get-output-string p) "\x1b;[?2026l") => #t)
)

;; 3. 类型错误检查
(check-catch 'type-error (terminal-end-sync! "not-a-terminal"))
(check-catch 'type-error (terminal-end-sync! 12345))

(check-report)
