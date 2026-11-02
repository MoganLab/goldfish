;; pi-app-hide-thinking?
;; 判定当前应用实例是否折叠隐藏助手思考过程内容。
;;
;; 语法
;; ----
;; (pi-app-hide-thinking? app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; boolean
;; 思考内容隐藏折叠时返回 #t，展开展示时返回 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认不折叠隐藏思考过程
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check-false (pi-app-hide-thinking? app))
) ;let

;; 2. 指定 :hide-thinking #t 时返回 #t
(let ((app (make-pi-app
             :terminal      (make-virtual-terminal :width 80 :height 24)
             :hide-thinking #t
           ) ;make-pi-app
      ) ;app
     ) ;
  (check-true (pi-app-hide-thinking? app))
) ;let

(check-report)
