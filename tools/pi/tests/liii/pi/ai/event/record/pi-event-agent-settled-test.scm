;; pi-event-agent-settled
;; 构造 agent_settled 事件对象。
;;
;; 语法
;; ----
;; (pi-event-agent-settled aborted)
;;
;; 参数
;; ----
;; aborted : boolean?
;; 是否由中止退出。
;;
;; 返回值
;; ----
;; pi-event-agent-settled?
;; Agent 结算事件 record。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(let ((ev (pi-event-agent-settled #f)))
  (check-true (pi-event-agent-settled? ev))
  (check (pi-event-agent-settled-type ev) => "agent_settled")
  (check (pi-event-agent-settled-aborted ev) => #f)
) ;let

(check-report)
