(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-event-turn-start
;; 构造 turn_start 事件对象。
(let ((ev (pi-event-turn-start)))
  (check (cdr (assoc "type" ev)) => "turn_start"))

(check-report)
