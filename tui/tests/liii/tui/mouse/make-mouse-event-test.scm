;; make-mouse-event
;; 构造鼠标事件对象，包含事件类型、按钮、坐标、修饰键以及发生时间戳。
;;
;; 语法
;; ----
;; (make-mouse-event type button x y [shift? ctrl? alt? time])
;;
;; 参数
;; ----
;; type : symbol?
;; 事件类型，如 'press, 'release, 'click, 'drag, 'wheel-up, 'wheel-down 等。
;;
;; button : symbol?
;; 鼠标按钮，如 'left, 'middle, 'right, 'none 等。
;;
;; x : integer?
;; 终端列坐标（0-based）。
;;
;; y : integer?
;; 终端行坐标（0-based）。
;;
;; shift? : boolean? (可选，默认为 #f)
;; Shift 键是否处于按下状态。
;;
;; ctrl? : boolean? (可选，默认为 #f)
;; Ctrl 键是否处于按下状态。
;;
;; alt? : boolean? (可选，默认为 #f)
;; Alt 键是否处于按下状态。
;;
;; time : number? (可选，默认为 #f)
;; 事件发生的时间戳（毫秒）。
;;
;; 返回值
;; ----
;; mouse-event?
;; 构造得到的鼠标事件对象。

(import (scheme base) (liii check) (liii tui mouse))

(check-set-mode! 'report-failed)

;; 基础构造
(let ((e (make-mouse-event 'press 'left 10 20)))
  (check-true (mouse-event? e))
  (check (mouse-event-type e) => 'press)
  (check (mouse-event-button e) => 'left)
  (check (mouse-event-x e) => 10)
  (check (mouse-event-y e) => 20)
  (check-false (mouse-event-shift? e))
  (check-false (mouse-event-ctrl? e))
  (check-false (mouse-event-alt? e))
  (check-false (mouse-event-time e))
) ;let

;; 带修饰键与时间戳构造
(let ((e (make-mouse-event 'click 'right 5 6 #t #f #t 12345)))
  (check (mouse-event-type e) => 'click)
  (check (mouse-event-button e) => 'right)
  (check (mouse-event-x e) => 5)
  (check (mouse-event-y e) => 6)
  (check-true (mouse-event-shift? e))
  (check-false (mouse-event-ctrl? e))
  (check-true (mouse-event-alt? e))
  (check (mouse-event-time e) => 12345)
) ;let

(check-report)
