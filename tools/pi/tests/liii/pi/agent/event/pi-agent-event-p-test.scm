;; pi-agent-event?
;; 判断对象是否为 Agent 事件 record。
;;
;; 语法
;; ----
;; (pi-agent-event? x)
;;
;; 参数
;; ----
;; x : any
;; 任意 Scheme 对象。
;;
;; 返回值
;; ----
;; boolean?
;; 若为 10 种 Agent 事件之一则返回 #t，否则返回 #f。

(import (liii check) (liii pi agent event))

(check-set-mode! 'report-failed)

(check-true (pi-agent-event? (pi-event-agent-start)))
(check-true (pi-agent-event? (pi-event-turn-start)))
(check-false (pi-agent-event? '()))

(check-report)
