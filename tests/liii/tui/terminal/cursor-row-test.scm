(import (scheme base)
        (liii check)
        (liii os)
        (liii tui))

(check-set-mode! 'report-failed)

;; console-cursor-row：TTY 下返回 0-based 非负整数，非 TTY 返回 #f。
;; 测试环境下 stdin 未必是 TTY，两种结果都合法，只验证形态。
(let ((r (console-cursor-row)))
  (check-true (or (not r) (and (integer? r) (>= r 0))))
) ;let

(check-report)
