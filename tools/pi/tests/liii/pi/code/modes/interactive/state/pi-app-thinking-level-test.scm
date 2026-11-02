;; pi-app-thinking-level
;; 获取当前应用实例生效的思考级别。
;;
;; 语法
;; ----
;; (pi-app-thinking-level app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 当前思考级别名称（string 或 #f）。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证获取到的思考级别
(let* ((app (make-pi-app
              :terminal       (make-virtual-terminal :width 80 :height 24)
              :thinking-level "medium"
            ) ;make-pi-app
       ) ;app
       (lvl (pi-app-thinking-level app))
      ) ;
  (check lvl => "medium")
) ;let*

(check-report)
