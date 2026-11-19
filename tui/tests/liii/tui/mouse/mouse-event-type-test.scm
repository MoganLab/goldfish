;; mouse-event-type
;; 获取鼠标事件的类型符号。
;;
;; 语法
;; ----
;; (mouse-event-type e)
;;
;; 参数
;; ----
;; e : mouse-event?
;; 鼠标事件对象。
;;
;; 返回值
;; ----
;; symbol?
;; 事件类型，如 'press, 'release, 'click, 'drag, 'wheel-up, 'wheel-down 等。

(import (scheme base)
  (liii check)
  (liii tui mouse)
) ;import

(check-set-mode! 'report-failed)

(let ((e (make-mouse-event 'press 'left 1 2)))
  (check (mouse-event-type e) => 'press)
) ;let

(check-report)
