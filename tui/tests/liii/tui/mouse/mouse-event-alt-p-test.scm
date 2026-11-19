;; mouse-event-alt?
;; 查询鼠标事件发生时 Alt 修饰键是否处于按下状态。
;;
;; 语法
;; ----
;; (mouse-event-alt? e)
;;
;; 参数
;; ----
;; e : mouse-event?
;; 鼠标事件对象。
;;
;; 返回值
;; ----
;; boolean?
;; Alt 按下返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui mouse)
) ;import

(check-set-mode! 'report-failed)

(let ((e (make-mouse-event 'press 'left 0 0 #f #f #t)))
  (check-true (mouse-event-alt? e))
) ;let

(let ((e (make-mouse-event 'press 'left 0 0 #f #f #f)))
  (check-false (mouse-event-alt? e))
) ;let

(check-report)
