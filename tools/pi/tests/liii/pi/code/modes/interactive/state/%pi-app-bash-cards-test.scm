;; %pi-app-bash-cards
;; 获取应用状态字段 %pi-app-bash-cards。
;;
;; 语法
;; ----
;; (%pi-app-bash-cards app)
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
  (check-true
    (let ((_ (%pi-app-bash-cards app)))
      #t
    ) ;let
  ) ;check-true
) ;let

(check-report)
