;; %make-pi-event-turn-start
;; 构造单轮回合开始事件 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-event-turn-start)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; pi-event-turn-start?
;; 单轮回合开始事件 record 实例。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-turn-start)))
  (check-true (pi-event-turn-start? obj))
) ;let

(check-report)
