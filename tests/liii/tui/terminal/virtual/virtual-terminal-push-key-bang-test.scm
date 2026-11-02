;; virtual-terminal-push-key!
;; 向虚拟终端的输入队列尾部追加一个按键或鼠标事件。
;;
;; 语法
;; ----
;; (virtual-terminal-push-key! vt key)
;;
;; 参数
;; ----
;; vt : virtual-terminal?
;; 虚拟终端实例。
;;
;; key : any
;; 要入队的按键符号、字符串或 mouse-event 对象。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui mouse) (liii tui terminal))

(check-set-mode! 'report-failed)

(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (virtual-terminal-push-key! vt 'up)
  (virtual-terminal-push-key! vt 'down)
  ;; 按 FIFO 顺序弹出
  (check (virtual-terminal-pop-key! vt) => 'up)
  (check (virtual-terminal-pop-key! vt) => 'down)
  (check (virtual-terminal-pop-key! vt) => #f)
) ;let

;; 鼠标事件也可入队（未带时刻时入队会自动补打当前单调时钟）
(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (let ((ev (make-mouse-event 'click 'left 1 2)))
    (virtual-terminal-push-key! vt ev)
    (let ((popped (virtual-terminal-pop-key! vt)))
      (check (mouse-event? popped) => #t)
      (check (mouse-event-type popped) => 'click)
      (check (mouse-event-button popped) => 'left)
      (check (mouse-event-x popped) => 1)
      (check (mouse-event-y popped) => 2)
    ) ;let
  ) ;let
) ;let

(check-catch 'type-error (virtual-terminal-push-key! "not-a-vt" 'up))

(check-report)
