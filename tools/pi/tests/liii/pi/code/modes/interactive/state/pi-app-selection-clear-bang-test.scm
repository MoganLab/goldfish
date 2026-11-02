;; pi-app-selection-clear!
;; 清空应用当前的选区状态（设置为 #f）。
;;
;; 语法
;; ----
;; (pi-app-selection-clear! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; -----
;; unspecified

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
      ) ;
  (pi-app-selection-set! app sel)
  (check-true (pi-app-selection-active? app))
  (pi-app-selection-clear! app)
  (check (pi-app-selection app) => #f)
  (check-false (pi-app-selection-active? app))
) ;let*

(check-report)
