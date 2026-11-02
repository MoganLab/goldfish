;; pi-app-apply-thinking-level-ui!
;; 更新应用的思考等级状态、footer 显示与编辑器边框颜色。
;;
;; 语法
;; ----
;; (pi-app-apply-thinking-level-ui! app level)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; level : string?
;; 目标思考等级（如 "off"、"low"、"medium"、"high"）。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive app-thinking)
) ;import

(check-set-mode! 'report-failed)

;; 1. 应用 thinking level 更新 state
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (pi-app-apply-thinking-level-ui! app "high")
  (check (pi-app-thinking-level app) => "high")
) ;let*

(check-report)
