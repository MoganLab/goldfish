(import (scheme base) (liii check) (liii string) (liii tui) (liii pi code modes interactive views))

(check-set-mode! 'report-failed)

;; 等待队列提示卡片使用 dim 前景
(let* ((queued-card (make-queued-card "等待执行的后续指令"))
       (w 40)
       (lines (component-render queued-card w)))
  (check (component? queued-card) => #t)
  (check (string-contains? (car lines) "Queued") => #t)
  (check (string-contains? (car lines) "等待执行的后续指令") => #t)
  (check (string-contains? (car lines) "38;2;126;136;142") => #t))

(check-report)
