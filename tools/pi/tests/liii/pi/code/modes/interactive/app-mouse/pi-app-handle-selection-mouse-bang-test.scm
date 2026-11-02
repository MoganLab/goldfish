;; pi-app-handle-selection-mouse!
;; 处理终端鼠标左键拖拽选区状态机（按压、拖拽、释放与点击清除）。
;;
;; 语法
;; ----
;; (pi-app-handle-selection-mouse! app ev)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; ev : mouse-event?
;; 鼠标事件对象。
;;
;; 返回值
;; -----
;; boolean?
;; 若选区状态发生变更需触发重绘则返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii tui mouse)
  (liii pi code modes interactive app)
  (liii pi code modes interactive app-mouse)
  (liii pi code modes interactive state)
  (liii pi code modes interactive selection)
) ;import

(check-set-mode! 'report-failed)

;; 1. press: 记录锚点，初始未激活
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check-false (pi-app-selection-active? app))
  (check-false (pi-app-handle-selection-mouse! app (make-mouse-event 'press 'left 5 5))
  ) ;check-false
  (let ((sel (pi-app-selection app)))
    (check (selection-anchor sel) => '(5 . 5))
    (check (selection-focus sel) => '(5 . 5))
    (check-false (selection-active? sel))
  ) ;let
  ;; 2. drag: 产生位移时激活选区，返回 #t
  (check-true (pi-app-handle-selection-mouse! app (make-mouse-event 'drag 'left 15 8))
  ) ;check-true
  (check-true (pi-app-selection-active? app))
  (let ((sel (pi-app-selection app)))
    (check (selection-anchor sel) => '(5 . 5))
    (check (selection-focus sel) => '(15 . 8))
  ) ;let
  ;; 3. release: 释放结束
  (check-true (pi-app-handle-selection-mouse! app (make-mouse-event 'release 'left 15 8))
  ) ;check-true
  (check-true (pi-app-selection-active? app))
  ;; 4. click: 清除已有选区并返回 #t
  (check-true (pi-app-handle-selection-mouse! app (make-mouse-event 'click 'left 0 0))
  ) ;check-true
  (check-false (pi-app-selection-active? app))
) ;let

(check-report)
