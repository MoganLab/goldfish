;; mouse-event-time
;; 获取鼠标事件发生的单调时间戳（毫秒）。
;;
;; 语法
;; ----
;; (mouse-event-time e)
;;
;; 参数
;; ----
;; e : mouse-event?
;; 鼠标事件对象。
;;
;; 返回值
;; ----
;; number? 或 #f
;; 事件时间戳（毫秒）；未记录时返回 #f。

(import (scheme base)
  (liii check)
  (liii tui mouse)
) ;import

(check-set-mode! 'report-failed)

(let ((e (make-mouse-event 'press 'left 0 0 #f #f #f 123456)))
  (check (mouse-event-time e) => 123456)
) ;let

(let ((e (make-mouse-event 'press 'left 0 0)))
  (check-false (mouse-event-time e))
) ;let

(check-report)
