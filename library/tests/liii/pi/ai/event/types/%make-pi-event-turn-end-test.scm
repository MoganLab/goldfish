;; %make-pi-event-turn-end
;; 构造单轮回合结束事件 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-event-turn-end message tool-results)
;;
;; 参数
;; ----
;; message : any?
;; 本轮结束的助手消息
;;
;; tool-results : vector?
;; 本轮产生的工具结果列表
;;
;; 返回值
;; ----
;; pi-event-turn-end?
;; 单轮回合结束事件 record 实例。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-turn-end "turn 消息" #())))
  (check-true (pi-event-turn-end? obj))
  (check (pi-event-turn-end-message obj) => "turn 消息")
  (check (pi-event-turn-end-tool-results obj) => #())
) ;let

(check-report)
