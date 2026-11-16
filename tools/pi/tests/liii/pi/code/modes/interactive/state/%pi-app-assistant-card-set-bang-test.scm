;; %pi-app-assistant-card-set!
;; 设置应用状态字段 pi-app-assistant-card。
;;
;; 语法
;; ----
;; (%pi-app-assistant-card-set! app val)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; val : any
;; 待设置的卡片实例或 #f。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive views)
) ;import

(check-set-mode! 'report-failed)

(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
      (card (make-assistant-card "hi"))
     ) ;
  (%pi-app-assistant-card-set! app card)
  (check (pi-app-assistant-card app) => card)
  (%pi-app-assistant-card-set! app #f)
  (check (pi-app-assistant-card app) => #f)
) ;let

(check-report)
