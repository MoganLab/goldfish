;; pi-app-recovery-card
;; 读取当前 all-in-one 全链等待状态卡片（用于原地更新与结算清理）。
;;
;; 语法
;; ----
;; (pi-app-recovery-card app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ----
;; (or component #f)
;; 当前等待卡片；未在等待时为 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive status-card)
) ;import

(check-set-mode! 'report-failed)

;; 1. 初始无等待卡片
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check-false (pi-app-recovery-card app))
) ;let

;; 2. 设置后读回同一组件，清空后回到 #f
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (card (make-status-card "waiting"))
      ) ;
  (%pi-app-recovery-card-set! app card)
  (check (pi-app-recovery-card app) => card)
  (%pi-app-recovery-card-set! app #f)
  (check-false (pi-app-recovery-card app))
) ;let*

(check-report)
