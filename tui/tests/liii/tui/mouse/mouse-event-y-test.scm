;; mouse-event-y
;; 获取鼠标事件发生的身坐标行号（0-based）。
;;
;; 语法
;; ----
;; (mouse-event-y e)
;;
;; 参数
;; ----
;; e : mouse-event?
;; 鼠标事件对象。
;;
;; 返回值
;; ----
;; integer?
;; 0-based 行坐标。

(import (scheme base)
  (liii check)
  (liii tui mouse)
) ;import

(check-set-mode! 'report-failed)

(let ((e (make-mouse-event 'press 'left 15 20)))
  (check (mouse-event-y e) => 20)
) ;let

(check-report)
