(import (liii check) (liii pi ai event))

(check-set-mode! 'report-failed)

;; pi-build-json-event
;; 根据事件类型统一分发构造对应的事件对象。
(let ((ev (pi-build-json-event 'agent_start '())))
  (check (cdr (assoc "type" ev)) => "agent_start"))

(check-report)
