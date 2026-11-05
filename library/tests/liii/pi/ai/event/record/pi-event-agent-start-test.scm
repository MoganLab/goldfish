;; pi-event-agent-start
;; 构造 agent_start 事件对象。
;;
;; 语法
;; ----
;; (pi-event-agent-start)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; pi-event-agent-start?
;; Agent 开始事件 record。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(let ((ev (pi-event-agent-start)))
  (check-true (pi-event-agent-start? ev))
  (check (pi-event-agent-start-type ev) => "agent_start")
) ;let

(check-report)
