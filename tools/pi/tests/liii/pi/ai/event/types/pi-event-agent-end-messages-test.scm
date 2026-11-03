;; pi-event-agent-end-messages
;; 获取 Agent 循环结束事件 record 的 messages 字段。
;;
;; 语法
;; ----
;; (pi-event-agent-end-messages obj)
;;
;; 参数
;; ----
;; obj : any?
;;  Agent 循环结束事件 record 实例。
;;
;; 返回值
;; ----
;; list?
;; messages 字段的值。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-event-agent-end '("m1" "m2") #f '((total . 0.01)) #f))
       (obj-b (%make-pi-event-agent-end '("m3") #t #f '((cached . 0.02))))
      ) ;
  (check (pi-event-agent-end-messages obj-a) => '("m1" "m2"))
  (check (pi-event-agent-end-messages obj-b) => '("m3"))
) ;let*

(check-report)
