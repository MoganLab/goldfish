;; pi-event-agent-end
;; 构造 agent_end 事件对象。
;;
;; 语法
;; ----
;; (pi-event-agent-end messages will-retry)
;; (pi-event-agent-end messages will-retry cost savings)
;;
;; 参数
;; ----
;; messages : list?
;; 当前全部消息列表。
;;
;; will-retry : boolean?
;; 是否将重试。
;;
;; cost : number?
;; 可选，费用。
;;
;; savings : number?
;; 可选，节省费用。
;;
;; 返回值
;; ----
;; pi-event-agent-end?
;; Agent 循环结束事件 record。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(let ((ev (pi-event-agent-end '() #f 0.5 0.2)))
  (check-true (pi-event-agent-end? ev))
  (check (pi-event-agent-end-type ev) => "agent_end")
  (check (pi-event-agent-end-will-retry ev) => #f)
  (check (pi-event-agent-end-cost ev) => 0.5)
  (check (pi-event-agent-end-savings ev) => 0.2)
) ;let

(check-report)
