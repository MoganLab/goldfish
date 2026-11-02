;; pi-app-close-selector!
;; 关闭当前激活的选择器弹出层并恢复编辑器焦点。
;;
;; 语法
;; ----
;; (pi-app-close-selector! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
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

;; 1. 关闭选择器后 active-selector 置空且恢复编辑器焦点
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (%pi-app-active-selector-set! app "dummy-selector")
  (editor-set-focused! (pi-app-editor app) #f)
  (pi-app-close-selector! app)
  (check (pi-app-active-selector app) => #f)
  (check-true (editor-focused? (pi-app-editor app)))
) ;let*

(check-report)
