(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-event-session
;; 构造 session header 事件对象。
(let ((ev (pi-event-session "/tmp/cwd")))
  (check-true (pi-event-session? ev))
  (check (pi-event-session-type ev) => "session")
  (check (pi-event-session-version ev) => 3)
  (check (pi-event-session-cwd ev) => "/tmp/cwd")
  (check-true (string? (pi-event-session-id ev)))
  (check-true (string? (pi-event-session-timestamp ev)))
  ;; 兼容 alist 访问
  (check (cdr (assoc "type" ev)) => "session")
  (check (cdr (assoc "version" ev)) => 3)
  (check (cdr (assoc "cwd" ev)) => "/tmp/cwd"))

;; 指定 id 与 timestamp 构造
(let ((ev (pi-event-session "/tmp/cwd" "custom-id" "2026-10-18T00:00:00.000Z")))
  (check-true (pi-event-session? ev))
  (check (pi-event-session-id ev) => "custom-id")
  (check (pi-event-session-timestamp ev) => "2026-10-18T00:00:00.000Z"))

(check-report)
