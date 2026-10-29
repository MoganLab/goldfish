(import (liii check)
        (liii tui mouse)
        (liii tui terminal)
        (liii tui terminal poll))

(check-set-mode! 'report-failed)

;; terminal-drain-mouse-events
;; 一次性取走当前已到达的所有鼠标事件（非阻塞）。
;;
;; 语法
;; ----
;; (terminal-drain-mouse-events [term])
;;
;; 参数
;; ----
;; term : terminal?
;; 目标终端，缺省为当前标准终端。
;;
;; 返回值
;; ----
;; list of mouse-event
;; 已到达的鼠标事件列表（按到达顺序）。
;;
;; 注意
;; ----
;; 遇到非鼠标按键立即停止，并把该按键原样放回输入队列头部：
;; 既不丢按键，也不改变按键顺序。
;; 典型用法：滚轮事件连续到达时一次性取走、合并成一次滚动，
;; 避免渲染耗时大于事件到达间隔时输入不断积压。

(define (make-wheel-up) (make-mouse-event 'wheel-up 'none 5 5))
(define (make-wheel-down) (make-mouse-event 'wheel-down 'none 5 5))

;; 1. 连续鼠标事件被一次取走
(let ((vt (make-virtual-terminal 40 20)))
  (virtual-terminal-push-key! vt (make-wheel-up))
  (virtual-terminal-push-key! vt (make-wheel-down))
  (let ((events (terminal-drain-mouse-events vt)))
    (check (length events) => 2)
    (check (mouse-event-type (car events)) => 'wheel-up)
    (check (mouse-event-type (cadr events)) => 'wheel-down)
  ) ;let
  ;; 取走后队列为空
  (check (terminal-poll-key vt) => #f)
) ;let

;; 2. 非鼠标按键被放回队列头部（顺序不变、不丢失）
(let ((vt (make-virtual-terminal 40 20)))
  (virtual-terminal-push-key! vt (make-wheel-up))
  (virtual-terminal-push-key! vt 'up)
  (virtual-terminal-push-key! vt (make-wheel-down))
  (let ((events (terminal-drain-mouse-events vt)))
    ;; 只取到第一个鼠标事件：其后是普通按键，停止并放回
    (check (length events) => 1)
    (check (mouse-event-type (car events)) => 'wheel-up)
  ) ;let
  (check (terminal-poll-key vt) => 'up)
  ;; 逐字段断言：虚拟终端会为鼠标事件补打到达时刻，整个 record 不再字面相等
  (let ((ev (terminal-poll-key vt)))
    (check-true (mouse-event? ev))
    (check (mouse-event-type ev) => 'wheel-down)
    (check (mouse-event-button ev) => 'none)
    (check (mouse-event-x ev) => 5)
    (check (mouse-event-y ev) => 5)
  ) ;let
) ;let

;; 3. 首个按键就是普通按键：不取任何鼠标事件，且按键保留
(let ((vt (make-virtual-terminal 40 20)))
  (virtual-terminal-push-key! vt "w")
  (virtual-terminal-push-key! vt (make-wheel-up))
  (check (terminal-drain-mouse-events vt) => '())
  (check (terminal-poll-key vt) => "w")
  (check (mouse-event-type (terminal-poll-key vt)) => 'wheel-up)
) ;let

;; 4. 无输入时返回空列表
(let ((vt (make-virtual-terminal 40 20)))
  (check (terminal-drain-mouse-events vt) => '())
) ;let

;; 5. 类型检查
(check-catch 'type-error (terminal-drain-mouse-events 123))
(check-catch 'type-error (terminal-drain-mouse-events "invalid"))

(check-report)
