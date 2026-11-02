;; pi-app-selection-active?
;; 检查应用当前是否拥有激活的文本选区。
;;
;; 语法
;; ----
;; (pi-app-selection-active? app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; -----
;; boolean?
;; 若选区存在且已激活则返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive selection)
) ;import

(check-set-mode! 'report-failed)

(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (sel (make-selection-range '(0 . 0) '(3 . 3) #t))
       (sel-inactive (make-selection-range '(0 . 0) '(3 . 3) #f))
      ) ;
  (check-false (pi-app-selection-active? app))
  (pi-app-selection-set! app sel-inactive)
  (check-false (pi-app-selection-active? app))
  (pi-app-selection-set! app sel)
  (check-true (pi-app-selection-active? app))
  (pi-app-selection-clear! app)
  (check-false (pi-app-selection-active? app))
) ;let*

(check-report)
