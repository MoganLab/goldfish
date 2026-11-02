;; pi-app-provider
;; 获取当前应用实例使用的模型供应商名称。
;;
;; 语法
;; ----
;; (pi-app-provider app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 供应商名称（string 或 #f）。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证获取指定的 provider
(let* ((app (make-pi-app
              :terminal (make-virtual-terminal :width 80 :height 24)
              :provider "anthropic"
            ) ;make-pi-app
       ) ;app
      ) ;
  (check (pi-app-provider app) => "anthropic")
) ;let*

(check-report)
