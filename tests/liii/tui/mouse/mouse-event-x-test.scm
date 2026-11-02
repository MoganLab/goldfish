;; mouse-event-x
;; 获取鼠标事件发生的列坐标（0-based）。
;;
;; 语法
;; ----
;; (mouse-event-x e)
;;
;; 参数
;; ----
;; e : mouse-event?
;; 鼠标事件对象。
;;
;; 返回值
;; ----
;; integer?
;; 0-based 列坐标。

(import (scheme base)
  (liii check)
  (liii tui mouse)
) ;import

(check-set-mode! 'report-failed)

(let ((e (make-mouse-event 'press 'left 15 20)))
  (check (mouse-event-x e) => 15)
) ;let

(check-report)
