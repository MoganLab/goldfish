;; pi-event-turn-end
;; 构造 turn_end 事件对象。
;;
;; 语法
;; ----
;; (pi-event-turn-end message tool-results)
;;
;; 参数
;; ----
;; message : any?
;; 回合完成后的消息对象。
;;
;; tool-results : any?
;; 工具执行结果。
;;
;; 返回值
;; ----
;; pi-event-turn-end?
;; 回合结束事件 record。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(let ((ev (pi-event-turn-end "msg" #())))
  (check-true (pi-event-turn-end? ev))
  (check (pi-event-turn-end-type ev) => "turn_end")
  (check (pi-event-turn-end-message ev) => "msg")
) ;let

(check-report)
