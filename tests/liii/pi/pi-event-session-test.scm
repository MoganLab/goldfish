(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-event-session
;; 构造 session header 事件对象。
(let ((ev (pi-event-session "/tmp/cwd")))
  (check (cdr (assoc "type" ev)) => "session")
  (check (cdr (assoc "version" ev)) => 3)
  (check (cdr (assoc "cwd" ev)) => "/tmp/cwd"))

(check-report)
