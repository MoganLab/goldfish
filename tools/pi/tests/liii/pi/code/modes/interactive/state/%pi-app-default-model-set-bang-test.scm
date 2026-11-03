;; %pi-app-default-model-set!
;; 设置应用状态持有的默认模型标记快照。
;;
;; 语法
;; ----
;; (%pi-app-default-model-set! app default-model)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; default-model : any
;; 新的默认模型条目对（如 ((provider . "p") (id . "m"))）或 #f。
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

;; 1. 验证更新应用持有的默认模型快照
(let* ((initial-dm '((provider . "google") (id . "gemini-3.8-flash")))
       (new-dm '((provider . "kimi-coding") (id . "k3.1-pro")))
       (app (make-pi-app
              :terminal      (make-virtual-terminal :width 80 :height 24)
              :default-model initial-dm
            ) ;make-pi-app
       ) ;app
      ) ;
  (check (%pi-app-default-model app) => initial-dm)
  (%pi-app-default-model-set! app new-dm)
  (check (%pi-app-default-model app) => new-dm)
  (%pi-app-default-model-set! app #f)
  (check (%pi-app-default-model app) => #f)
) ;let*

(check-report)
