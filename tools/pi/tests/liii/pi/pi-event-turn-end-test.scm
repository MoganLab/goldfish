(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-event-turn-end
;; 构造 turn_end 轮次结束事件对象。
(let* ((msg '(("role" . "assistant")))
       (tool-res #())
       (ev (pi-event-turn-end msg tool-res)))
  (check-true (pi-event-turn-end? ev))
  (check (pi-event-turn-end-type ev) => "turn_end")
  (check (pi-event-turn-end-message ev) => msg)
  (check (pi-event-turn-end-tool-results ev) => tool-res)
  ;; 兼容 alist 访问
  (check (cdr (assoc "type" ev)) => "turn_end")
  (check (cdr (assoc "message" ev)) => msg)
  (check (cdr (assoc "toolResults" ev)) => tool-res))

(check-report)
