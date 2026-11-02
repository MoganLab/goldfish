;; pi-app-tools-expanded?
;; 判定当前应用实例的工具详细输出是否展开。
;;
;; 语法
;; ----
;; (pi-app-tools-expanded? app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; boolean
;; 工具输出展开时返回 #t，折叠时返回 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认工具输出处于折叠状态 (#f)
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check-false (pi-app-tools-expanded? app))
) ;let

(check-report)
