;; pi-event-agent-settled-aborted
;; 获取 Agent 任务最终结算事件 record 的 aborted 字段。
;;
;; 语法
;; ----
;; (pi-event-agent-settled-aborted obj)
;;
;; 参数
;; ----
;; obj : any?
;;  Agent 任务最终结算事件 record 实例。
;;
;; 返回值
;; ----
;; boolean?
;; aborted 字段的值。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-event-agent-settled #f))
       (obj-b (%make-pi-event-agent-settled #t))
      ) ;
  (check (pi-event-agent-settled-aborted obj-a) => #f)
  (check (pi-event-agent-settled-aborted obj-b) => #t)
) ;let*

(check-report)
