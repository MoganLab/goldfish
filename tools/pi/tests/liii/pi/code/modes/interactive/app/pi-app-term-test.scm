;; pi-app-term
;; 获取应用实例关联的终端对象。
;;
;; 语法
;; ----
;; (pi-app-term app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 终端对象。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证获取传入的终端对象
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt))
      ) ;
  (check (pi-app-term app) => vt)
) ;let*

(check-report)
