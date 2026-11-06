;; virtual-terminal-pop-key!
;; 从虚拟终端的输入队列头部取出一个按键或鼠标事件。
;;
;; 语法
;; ----
;; (virtual-terminal-pop-key! vt)
;;
;; 参数
;; ----
;; vt : virtual-terminal?
;; 虚拟终端实例。
;;
;; 返回值
;; ----
;; any 或 #f
;; 队列头部的按键；队列为空时返回 #f。

(import (scheme base) (liii check) (liii tui terminal))

(check-set-mode! 'report-failed)

;; 空队列返回 #f
(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (check (virtual-terminal-pop-key! vt) => #f)
) ;let

;; FIFO 顺序
(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (virtual-terminal-push-key! vt 'a)
  (virtual-terminal-push-key! vt 'b)
  (check (virtual-terminal-pop-key! vt) => 'a)
  (check (virtual-terminal-pop-key! vt) => 'b)
  (check (virtual-terminal-pop-key! vt) => #f)
) ;let

(check-catch 'type-error (virtual-terminal-pop-key! "not-a-vt"))

(check-report)
