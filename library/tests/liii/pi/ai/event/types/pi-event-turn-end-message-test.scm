;; pi-event-turn-end-message
;; 获取单轮回合结束事件 record 的 message 字段。
;;
;; 语法
;; ----
;; (pi-event-turn-end-message obj)
;;
;; 参数
;; ----
;; obj : any?
;; 单轮回合结束事件 record 实例。
;;
;; 返回值
;; ----
;; any?
;; message 字段的值。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-event-turn-end "turn 消息" #()))
       (obj-b (%make-pi-event-turn-end "other 消息" #("结果一" "结果二")))
      ) ;
  (check (pi-event-turn-end-message obj-a) => "turn 消息")
  (check (pi-event-turn-end-message obj-b) => "other 消息")
) ;let*

(check-report)
