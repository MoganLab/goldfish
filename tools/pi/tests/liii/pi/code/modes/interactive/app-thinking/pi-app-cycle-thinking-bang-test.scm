;; pi-app-cycle-thinking!
;; 循环切换应用当前的思考级别至下一个可用级别。
;;
;; 语法
;; ----
;; (pi-app-cycle-thinking! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 切换后的思考级别名称（string），若模型不支持思考则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 不支持思考的模型返回 #f
(let* ((app (make-pi-app
              :terminal          (make-virtual-terminal :width 80 :height 24)
              :supports-thinking #f
            ) ;make-pi-app
       ) ;app
      ) ;
  (check-false (pi-app-cycle-thinking! app))
) ;let*

;; 2. 支持思考的模型循环切换
(let* ((app (make-pi-app
              :terminal          (make-virtual-terminal :width 80 :height 24)
              :supports-thinking #t
              :thinking-level    "off"
              :available-thinking-levels '("off" "low" "medium" "high")
            ) ;make-pi-app
       ) ;app
      ) ;
  ;; Shift+Tab 切换属于用户显式意图
  (check-false (pi-app-thinking-level-explicit? app))
  (check (pi-app-cycle-thinking! app) => "low")
  (check (pi-app-thinking-level app) => "low")
  (check-true (pi-app-thinking-level-explicit? app))
) ;let*

(check-report)
