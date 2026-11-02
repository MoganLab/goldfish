;; mouse-event-click?
;; 判断鼠标事件对象是否为单击（click）事件。
;;
;; 语法
;; ----
;; (mouse-event-click? ev)
;;
;; 参数
;; ----
;; ev : mouse-event?
;; 待检查的鼠标事件对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果事件类型为 'click 返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui mouse))

(check-set-mode! 'report-failed)

(check-true (mouse-event-click? (make-mouse-event 'click 'left 0 0)))
(check-false (mouse-event-click? (make-mouse-event 'press 'left 0 0)))
(check-false (mouse-event-click? (make-mouse-event 'release 'left 0 0)))
(check-false (mouse-event-click? (make-mouse-event 'wheel-up 'none 0 0)))
(check-catch 'type-error (mouse-event-click? "not-event"))

(check-report)
