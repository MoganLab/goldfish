;; %make-pi-event-agent-settled
;; 构造 Agent 任务最终结算事件 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-event-agent-settled aborted)
;;
;; 参数
;; ----
;; aborted : boolean?
;; 任务是否被中止
;;
;; 返回值
;; ----
;; pi-event-agent-settled?
;; Agent 任务最终结算事件 record 实例。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-agent-settled #f)))
  (check-true (pi-event-agent-settled? obj))
  (check (pi-event-agent-settled-aborted obj) => #f)
) ;let

(check-report)
