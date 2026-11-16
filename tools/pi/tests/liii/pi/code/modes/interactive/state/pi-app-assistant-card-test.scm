;; pi-app-assistant-card
;; 获取应用状态字段 pi-app-assistant-card。
;;
;; 语法
;; ----
;; (pi-app-assistant-card app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ----
;; any
;; 当前字段存储的状态值。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check (pi-app-assistant-card app) => #f)
) ;let

(check-report)
