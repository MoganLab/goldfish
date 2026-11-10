;; mouse-event-button
;; 获取鼠标事件的按钮符号。
;;
;; 语法
;; ----
;; (mouse-event-button e)
;;
;; 参数
;; ----
;; e : mouse-event?
;; 鼠标事件对象。
;;
;; 返回值
;; ----
;; symbol?
;; 按钮名称，如 'left, 'middle, 'right, 'none 等。

(import (scheme base)
  (liii check)
  (liii tui mouse)
) ;import

(check-set-mode! 'report-failed)

(let ((e (make-mouse-event 'press 'middle 1 2)))
  (check (mouse-event-button e) => 'middle)
) ;let

(check-report)
