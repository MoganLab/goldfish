;; mouse-event-ctrl?
;; 查询鼠标事件发生时 Ctrl 修饰键是否处于按下状态。
;;
;; 语法
;; ----
;; (mouse-event-ctrl? e)
;;
;; 参数
;; ----
;; e : mouse-event?
;; 鼠标事件对象。
;;
;; 返回值
;; ----
;; boolean?
;; Ctrl 按下返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui mouse)
) ;import

(check-set-mode! 'report-failed)

(let ((e (make-mouse-event 'press 'left 0 0 #f #t #f)))
  (check-true (mouse-event-ctrl? e))
) ;let

(let ((e (make-mouse-event 'press 'left 0 0 #f #f #f)))
  (check-false (mouse-event-ctrl? e))
) ;let

(check-report)
