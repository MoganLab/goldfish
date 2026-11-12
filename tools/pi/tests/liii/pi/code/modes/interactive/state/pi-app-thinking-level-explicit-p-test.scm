;; pi-app-thinking-level-explicit?
;; 读取当前思考等级是否来自用户运行时显式指定。
;;
;; 语法
;; ----
;; (pi-app-thinking-level-explicit? app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ----
;; boolean
;; 用户显式指定（CLI --thinking / /thinking / 选择器 / Shift+Tab）时为 #t；
;; 仅来自 settings 默认值或切模回退时为 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认构造：非显式
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check-false (pi-app-thinking-level-explicit? app))
) ;let

;; 2. CLI 显式给出时启动即标记
(let ((app (make-pi-app
             :terminal                (make-virtual-terminal :width 80 :height 24)
             :thinking-level          "high"
             :thinking-level-explicit #t
           ) ;make-pi-app
          ) ;
     ) ;
  (check-true (pi-app-thinking-level-explicit? app))
) ;let

;; 3. 修改器可复位标记
(let ((app (make-pi-app
             :terminal                (make-virtual-terminal :width 80 :height 24)
             :thinking-level          "high"
             :thinking-level-explicit #t
           ) ;make-pi-app
          ) ;
     ) ;
  (%pi-app-thinking-level-explicit?-set! app #f)
  (check-false (pi-app-thinking-level-explicit? app))
) ;let

(check-report)
