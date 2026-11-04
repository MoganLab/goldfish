;; %pi-app-models-set!
;; 设置应用状态持有的已激活模型列表快照。
;;
;; 语法
;; ----
;; (%pi-app-models-set! app models)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; models : list
;; 新的模型条目列表。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证更新应用持有的模型列表快照
(let* ((initial-models '(((provider . "test") (id . "m1"))))
       (new-models '(((provider . "test") (id . "m2"))
                     ((provider . "test2") (id . "m3")))
       ) ;new-models
       (app (make-pi-app
              :terminal (make-virtual-terminal :width 80 :height 24)
              :models   initial-models
            ) ;make-pi-app
       ) ;app
      ) ;
  (check (pi-app-models app) => initial-models)
  (%pi-app-models-set! app new-models)
  (check (pi-app-models app) => new-models)
) ;let*

(check-report)
