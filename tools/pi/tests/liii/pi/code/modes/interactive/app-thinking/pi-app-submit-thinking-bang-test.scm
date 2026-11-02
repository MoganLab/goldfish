;; pi-app-submit-thinking!
;; 处理 /thinking 命令，按参数直接设置思考等级或打开等级选择器。
;;
;; 语法
;; ----
;; (pi-app-submit-thinking! app text)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; text : string?
;; 用户输入的完整 /thinking 命令文本。
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

;; 1. 带具体等级参数直接设置
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm :supports-thinking #t))
      ) ;
  (pi-app-submit-thinking! app "/thinking high")
  (check (pi-app-thinking-level app) => "high")
  (check (editor-text (pi-app-editor app)) => "")
) ;let*

;; 2. 无参数打开选择器
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm :supports-thinking #t))
      ) ;
  (pi-app-submit-thinking! app "/thinking")
  (check-true (and (pi-app-active-selector app) #t))
) ;let*

(check-report)
