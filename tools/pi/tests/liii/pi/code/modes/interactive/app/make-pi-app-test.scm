;; make-pi-app
;; 构造并初始化交互式应用实例。
;;
;; 语法
;; ----
;; (make-pi-app . kwargs)
;;
;; 参数
;; ----
;; kwargs : any
;; 关键字参数（如 :terminal, :handler, :provider, :model 等）。
;;
;; 返回值
;; ------
;; any
;; 初始化完成的交互式应用对象（满足 pi-app?）。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 成功构造应用实例
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :provider "google" :model "gemini-2.5-flash"))
      ) ;
  (check-true (pi-app? app))
  (check (pi-app-provider app) => "google")
  (check (pi-app-model app) => "gemini-2.5-flash")
) ;let*

(check-report)
