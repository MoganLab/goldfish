;; pi-app-apply-model-ui!
;; 应用指定的模型配置更新界面与相关状态（provider、model、footer 等）。
;;
;; 语法
;; ----
;; (pi-app-apply-model-ui! app model)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; model : list?
;; 模型配置 alist，包含 provider、id 等键值对。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive app-model)
) ;import

(check-set-mode! 'report-failed)

;; 1. 应用模型到 UI 状态
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
       (model '((provider . "anthropic")
                (id . "claude-3-5-sonnet")
                (reasoning . #t)))
      ) ;
  (pi-app-apply-model-ui! app model)
  (check (pi-app-provider app) => "anthropic")
  (check (pi-app-model app) => "claude-3-5-sonnet")
  (check-true (pi-app-supports-thinking? app))
) ;let*

;; 2. 应用不支持 reasoning 的模型
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
       (model '((provider . "google") (id . "gemini-flash") (reasoning . #f)))
      ) ;
  (pi-app-apply-model-ui! app model)
  (check (pi-app-provider app) => "google")
  (check (pi-app-model app) => "gemini-flash")
  (check-false (pi-app-supports-thinking? app))
) ;let*

(check-report)
