;; pi-app-models
;; 获取当前应用实例持有的已激活模型列表。
;;
;; 语法
;; ----
;; (pi-app-models app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; list
;; 已激活模型条目列表。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证获取传入的模型列表
(let* ((fixture-models '(((provider . "test") (id . "m1"))))
       (app (make-pi-app
              :terminal (make-virtual-terminal :width 80 :height 24)
              :models   fixture-models
            ) ;make-pi-app
       ) ;app
      ) ;
  (check (pi-app-models app) => fixture-models)
) ;let*

(check-report)
