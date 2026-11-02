;; pi-app-toggle-thinking!
;; 翻转应用中思考过程卡片的折叠与展开隐藏状态。
;;
;; 语法
;; ----
;; (pi-app-toggle-thinking! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; boolean
;; 翻转后的思考折叠隐藏状态（隐藏时为 #t，展开时为 #f）。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认状态 (#f) 翻转为 #t
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check-false (pi-app-hide-thinking? app))
  (check-true (pi-app-toggle-thinking! app))
  (check-true (pi-app-hide-thinking? app))
  ;; 再次翻转回到 #f
  (check-false (pi-app-toggle-thinking! app))
  (check-false (pi-app-hide-thinking? app))
) ;let

(check-report)
