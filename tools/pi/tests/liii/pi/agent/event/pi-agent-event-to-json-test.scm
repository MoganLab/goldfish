;; pi-agent-event->json
;; 将 Agent 事件 record 转换为 JSON 对象 alist。
;;
;; 语法
;; ----
;; (pi-agent-event->json ev)
;;
;; 参数
;; ----
;; ev : pi-agent-event?
;; Agent 事件对象。
;;
;; 返回值
;; ----
;; list?
;; 对应的 JSON 对象 alist。

(import (liii check) (liii pi agent event))

(check-set-mode! 'report-failed)

(let ((j (pi-agent-event->json (pi-event-agent-start))))
  (check (cdr (assoc "type" j)) => "agent_start")
) ;let

(check-report)
