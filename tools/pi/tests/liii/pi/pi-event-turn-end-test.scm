(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-event-turn-end
;; 构造 turn_end 轮次结束事件对象。
(let ((ev (pi-event-turn-end '(("role" . "assistant")) #())))
  (check (cdr (assoc "type" ev)) => "turn_end"))

(check-report)
