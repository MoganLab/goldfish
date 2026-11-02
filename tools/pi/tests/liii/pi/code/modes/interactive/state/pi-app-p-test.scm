;; pi-app?
;; 判定指定对象是否为交互式应用实例。
;;
;; 语法
;; ----
;; (pi-app? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检验的对象。
;;
;; 返回值
;; ------
;; boolean
;; 为交互式应用实例时返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 正例判定
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check-true (pi-app? app))
) ;let

;; 2. 负例判定
(check-false (pi-app? 123))
(check-false (pi-app? "app"))
(check-false (pi-app? '()))

(check-report)
