;; mouse-event?
;; 判断给定对象是否为鼠标事件对象。
;;
;; 语法
;; ----
;; (mouse-event? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为鼠标事件对象返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui mouse)
) ;import

(check-set-mode! 'report-failed)

(let ((e (make-mouse-event 'press 'left 0 0)))
  (check-true (mouse-event? e))
) ;let

(check-false (mouse-event? "not-an-event"))
(check-false (mouse-event? 123))

(check-report)
