;; pi-event-turn-end-tool-results
;; 获取单轮回合结束事件 record 的 tool-results 字段。
;;
;; 语法
;; ----
;; (pi-event-turn-end-tool-results obj)
;;
;; 参数
;; ----
;; obj : any?
;; 单轮回合结束事件 record 实例。
;;
;; 返回值
;; ----
;; vector?
;; tool-results 字段的值。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-event-turn-end "turn 消息" #()))
       (obj-b (%make-pi-event-turn-end "other 消息" #("结果一" "结果二")))
      ) ;
  (check (pi-event-turn-end-tool-results obj-a) => #())
  (check (pi-event-turn-end-tool-results obj-b) => #("结果一" "结果二"))
) ;let*

(check-report)
