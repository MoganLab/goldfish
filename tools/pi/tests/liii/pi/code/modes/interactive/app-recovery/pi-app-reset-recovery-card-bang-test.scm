;; pi-app-reset-recovery-card!
;; 清除当前 all-in-one 等待状态卡片的引用（卡片本身留在对话记录中）。
;;
;; 语法
;; ----
;; (pi-app-reset-recovery-card! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ----
;; unspecified
;; 清除后后续 tick 会新建卡片；正常结算与恢复结束时调用。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive app-recovery)
  (liii pi code modes interactive status-card)
) ;import

(check-set-mode! 'report-failed)

;; 1. 清除后读回 #f
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (_ (%pi-app-recovery-card-set! app (make-status-card "waiting")))
      ) ;
  (pi-app-reset-recovery-card! app)
  (check-false (pi-app-recovery-card app))
) ;let*

;; 2. 空状态调用是安全的
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (pi-app-reset-recovery-card! app)
  (check-false (pi-app-recovery-card app))
) ;let

(check-report)
