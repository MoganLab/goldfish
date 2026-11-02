;; pi-app-bash-mode?
;; 判定当前输入编辑器是否处于 Bash 命令直发模式。
;;
;; 语法
;; ----
;; (pi-app-bash-mode? app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; boolean
;; 处于 Bash 命令直发模式时返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认处于非 Bash 模式
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check-false (pi-app-bash-mode? app))
) ;let

(check-report)
