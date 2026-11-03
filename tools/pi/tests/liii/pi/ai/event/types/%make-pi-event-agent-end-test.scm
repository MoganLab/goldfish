;; %make-pi-event-agent-end
;; 构造 Agent 循环结束事件 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-event-agent-end messages will-retry cost savings)
;;
;; 参数
;; ----
;; messages : list?
;; 本轮产生的消息列表
;;
;; will-retry : boolean?
;; 是否将继续重试
;;
;; cost : alist? 或 #f
;; 花费统计，未知时为 #f
;;
;; savings : alist? 或 #f
;; 节省统计，未知时为 #f
;;
;; 返回值
;; ----
;; pi-event-agent-end?
;; Agent 循环结束事件 record 实例。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-agent-end '("m1" "m2") #f '((total . 0.01)) #f)))
  (check-true (pi-event-agent-end? obj))
  (check (pi-event-agent-end-messages obj) => '("m1" "m2"))
  (check (pi-event-agent-end-will-retry obj) => #f)
  (check (pi-event-agent-end-cost obj) => '((total . 0.01)))
  (check (pi-event-agent-end-savings obj) => #f)
) ;let

(check-report)
