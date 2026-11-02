;; pi-app-no-session?
;; 判定应用状态是否满足 pi-app-no-session?。
;;
;; 语法
;; ----
;; (pi-app-no-session? app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ----
;; boolean?
;; 满足条件返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check-true
    (let ((_ (pi-app-no-session? app)))
      #t
    ) ;let
  ) ;check-true
) ;let

(check-report)
