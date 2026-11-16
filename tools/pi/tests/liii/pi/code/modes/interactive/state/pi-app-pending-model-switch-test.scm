;; pi-app-pending-model-switch
;; 读取请求进行中通过 /model 待切换的目标模型标签（#f 表示无）。
;;
;; 语法
;; ----
;; (pi-app-pending-model-switch app)
;;
;; 参数
;; ----
;; app : pi-app?
;;
;; 返回值
;; ----
;; (or string #f)

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report)

(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check (pi-app-pending-model-switch app) => #f)
  (%pi-app-pending-model-switch-set! app "zai/glm-5")
  (check (pi-app-pending-model-switch app) => "zai/glm-5")
) ;let

(check-report)
