;; %pi-app-pending-model-switch-set!
;; 设置请求进行中通过 /model 待切换的目标模型标签。
;;
;; 语法
;; ----
;; (%pi-app-pending-model-switch-set! app label)
;;
;; 参数
;; ----
;; app : pi-app?
;; label : (or string #f)
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

(check-set-mode! 'report)

(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (%pi-app-pending-model-switch-set! app "google/gemini-4-hydrogen")
  (check (pi-app-pending-model-switch app) => "google/gemini-4-hydrogen")
  (%pi-app-pending-model-switch-set! app #f)
  (check (pi-app-pending-model-switch app) => #f)
) ;let

(check-report)
