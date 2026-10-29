(import (scheme base)
        (liii check)
        (liii tui mouse)
        (liii tui keys)
        (liii tui terminal)
        (liii tui terminal poll))

(check-set-mode! 'report-failed)

;; mouse-event-click? 与事件类型扩展
;; 1. make-mouse-event 可构造 click 类型
(let ((e (make-mouse-event 'click 'left 2 3)))
  (check (mouse-event-type e) => 'click)
  (check (mouse-event-button e) => 'left)
) ;let

;; 2. click 事件判定
(check (mouse-event-click? (make-mouse-event 'click 'left 0 0)) => #t)
(check (mouse-event-click? (make-mouse-event 'press 'left 0 0)) => #f)
(check (mouse-event-click? (make-mouse-event 'release 'left 0 0)) => #f)
(check (mouse-event-click? (make-mouse-event 'wheel-up 'none 0 0)) => #f)
(check-catch 'type-error (mouse-event-click? "not-event"))

;; poll 层合成 click：press + 同位置同按钮 release -> click
;; ESC[<0;10;5M = 左键按下，ESC[<0;10;5m = 左键释放（同位置）
(let ((r (%poll-mouse-feed '(27 91 60 48 59 49 48 59 53 77))))
  (check (mouse-event-type r) => 'press)
) ;let
(let ((r (%poll-mouse-feed '(27 91 60 48 59 49 48 59 53 109))))
  (check (mouse-event? r) => #t)
  (check (mouse-event-type r) => 'click)
  (check (mouse-event-button r) => 'left)
  (check (mouse-event-x r) => 9)
  (check (mouse-event-y r) => 4)
) ;let

;; press 后位置变化的 release：返回 release，不合成 click
(let ((r (%poll-mouse-feed '(27 91 60 48 59 49 48 59 53 77))))
  (check (mouse-event-type r) => 'press)
) ;let
(let ((r (%poll-mouse-feed '(27 91 60 48 59 50 48 59 54 109))))
  (check (mouse-event-type r) => 'release)
) ;let

;; press 后不同按钮的 release：返回 release，不合成 click
(let ((r (%poll-mouse-feed '(27 91 60 48 59 49 48 59 53 77))))
  (check (mouse-event-type r) => 'press)
) ;let
(let ((r (%poll-mouse-feed '(27 91 60 50 59 49 48 59 53 109))))
  (check (mouse-event-type r) => 'release)
) ;let

;; 无 press 前置的孤立 release：直接返回 release
(let ((r (%poll-mouse-feed '(27 91 60 48 59 49 48 59 53 109))))
  (check (mouse-event-type r) => 'release)
) ;let

;; release 之后新的一对 press/release 再次合成 click
(let ((r (%poll-mouse-feed '(27 91 60 48 59 51 48 59 53 77))))
  (check (mouse-event-type r) => 'press)
) ;let
(let ((r (%poll-mouse-feed '(27 91 60 48 59 51 48 59 53 109))))
  (check (mouse-event-type r) => 'click)
) ;let

;; 中键与右键的 click 合成：1=中键，2=右键
(let ((r (%poll-mouse-feed '(27 91 60 49 59 49 48 59 53 77))))
  (check (mouse-event-type r) => 'press)
) ;let
(let ((r (%poll-mouse-feed '(27 91 60 49 59 49 48 59 53 109))))
  (check (mouse-event-type r) => 'click)
  (check (mouse-event-button r) => 'middle)
) ;let
(let ((r (%poll-mouse-feed '(27 91 60 50 59 49 48 59 53 77))))
  (check (mouse-event-type r) => 'press)
) ;let
(let ((r (%poll-mouse-feed '(27 91 60 50 59 49 48 59 53 109))))
  (check (mouse-event-type r) => 'click)
  (check (mouse-event-button r) => 'right)
) ;let

;; 滚轮事件不参与合成，也不污染 press 状态
(let ((r (%poll-mouse-feed '(27 91 60 54 52 59 49 48 59 53 77))))
  (check (mouse-event-type r) => 'wheel-up)
) ;let
(let ((r (%poll-mouse-feed '(27 91 60 48 59 49 48 59 53 109))))
  (check (mouse-event-type r) => 'release)
) ;let

;; 合成的 click 携带 release 的坐标（与 press 相同时即点击点）
(let ((r (%poll-mouse-feed '(27 91 60 48 59 49 48 59 53 77))))
  (check (mouse-event-type r) => 'press)
) ;let
(let ((r (%poll-mouse-feed '(27 91 60 48 59 49 48 59 53 109))))
  (check (mouse-event-x r) => 9)
  (check (mouse-event-y r) => 4)
) ;let

;; 状态复位：连续 click 不串扰
(%poll-mouse-reset!)
(let ((r (%poll-mouse-feed '(27 91 60 48 59 49 48 59 53 77))))
  (check (mouse-event-type r) => 'press)
) ;let
(let ((r (%poll-mouse-feed '(27 91 60 48 59 49 48 59 53 109))))
  (check (mouse-event-type r) => 'click)
) ;let

(check-report)
