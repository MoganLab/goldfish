;; pi-app-supports-thinking?
;; 判定当前应用所选模型是否支持深度思考。
;;
;; 语法
;; ----
;; (pi-app-supports-thinking? app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; boolean
;; 支持思考时返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认应用实例返回 boolean
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check-true (boolean? (pi-app-supports-thinking? app)))
) ;let*

(check-report)
