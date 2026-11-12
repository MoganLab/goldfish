;; %pi-app-thinking-level-explicit?-set!
;; 设置思考等级是否为用户显式指定。
;;
;; 语法
;; ----
;; (%pi-app-thinking-level-explicit?-set! app val)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; val : boolean
;; 是否为用户显式指定的思考等级。
;;
;; 返回值
;; ----
;; unspecified
;; 仅更新状态字段，不触发渲染。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

;; 1. 置位后可读回 #t
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (%pi-app-thinking-level-explicit?-set! app #t)
  (check-true (pi-app-thinking-level-explicit? app))
) ;let

;; 2. 复位为 #f
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (%pi-app-thinking-level-explicit?-set! app #t)
  (%pi-app-thinking-level-explicit?-set! app #f)
  (check-false (pi-app-thinking-level-explicit? app))
) ;let

(check-report)
