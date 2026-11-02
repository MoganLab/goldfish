;; pi-app-default-thinking-level
;; 获取当前应用实例配置的默认思考级别。
;;
;; 语法
;; ----
;; (pi-app-default-thinking-level app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 默认思考级别名称（string 或 #f）。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证可访问默认思考级别
(let* ((app (make-pi-app
              :terminal (make-virtual-terminal :width 80 :height 24)
              :default-thinking-level "low"
            ) ;make-pi-app
       ) ;app
       (lvl (pi-app-default-thinking-level app))
      ) ;
  (check lvl => "low")
) ;let*

(check-report)
