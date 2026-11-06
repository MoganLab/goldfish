;; virtual-terminal-unshift-key!
;; 把一个按键原样插回虚拟终端输入队列的头部。
;;
;; 排空输入时遇到非鼠标按键需要原样放回，保持按键顺序不变
;; （pop 从头部取，push 到尾部会改变顺序）。
;;
;; 语法
;; ----
;; (virtual-terminal-unshift-key! vt key)
;;
;; 参数
;; ----
;; vt : virtual-terminal?
;; 虚拟终端实例。
;;
;; key : any
;; 要插回队首的按键。
;;
;; 返回值
;; ----
;; any
;; 插回的按键本身。

(import (scheme base) (liii check) (liii tui terminal))

(check-set-mode! 'report-failed)

(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (virtual-terminal-push-key! vt 'b)
  (virtual-terminal-unshift-key! vt 'a)
  ;; unshift 插到队首，先弹出的是 'a
  (check (virtual-terminal-pop-key! vt) => 'a)
  (check (virtual-terminal-pop-key! vt) => 'b)
) ;let

;; unshift 返回按键本身
(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (check (virtual-terminal-unshift-key! vt 'x) => 'x)
) ;let

(check-catch 'type-error (virtual-terminal-unshift-key! "not-a-vt" 'x))

(check-report)
