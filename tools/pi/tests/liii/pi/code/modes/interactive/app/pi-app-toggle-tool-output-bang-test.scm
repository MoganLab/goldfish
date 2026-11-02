;; pi-app-toggle-tool-output!
;; 切换应用中工具输出的折叠与展开状态。
;;
;; 语法
;; ----
;; (pi-app-toggle-tool-output! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; boolean
;; 切换后的工具展开状态（展开为 #t，折叠为 #f）。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认状态 (#f) 切换为 #t，再次切换回到 #f
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check-false (pi-app-tools-expanded? app))
  (check-true (pi-app-toggle-tool-output! app))
  (check-true (pi-app-tools-expanded? app))
  (check-false (pi-app-toggle-tool-output! app))
  (check-false (pi-app-tools-expanded? app))
) ;let

(check-report)
