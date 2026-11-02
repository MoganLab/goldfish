;; pi-app-model
;; 获取当前应用实例使用的模型标识字符串。
;;
;; 语法
;; ----
;; (pi-app-model app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 模型标识（string 或 #f）。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证获取指定的 model
(let* ((app (make-pi-app
              :terminal (make-virtual-terminal :width 80 :height 24)
              :model    "claude-sonnet-4"
            ) ;make-pi-app
       ) ;app
      ) ;
  (check (pi-app-model app) => "claude-sonnet-4")
) ;let*

(check-report)
