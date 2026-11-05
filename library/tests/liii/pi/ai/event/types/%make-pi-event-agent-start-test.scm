;; %make-pi-event-agent-start
;; 构造 Agent 循环启动事件 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-event-agent-start)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; pi-event-agent-start?
;; Agent 循环启动事件 record 实例。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-agent-start)))
  (check-true (pi-event-agent-start? obj))
) ;let

(check-report)
