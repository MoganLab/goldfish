;; pi-app-handle-input!
;; 处理终端输入按键并派发给当前激活的组件或编辑器。
;;
;; 语法
;; ----
;; (pi-app-handle-input! app key)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; key : any
;; 终端捕获的按键事件（字符、字符串或符号）。
;;
;; 返回值
;; ------
;; boolean
;; 按键被消费且触发了状态更新时返回 #t，未消费或忽略时返回 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 输入普通字符并验证编辑器文本更新
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
       (ed (pi-app-editor app))
      ) ;
  (check-true (pi-app-handle-input! app #\h))
  (check-true (pi-app-handle-input! app #\i))
  (check (editor-text ed) => "hi")
) ;let*

(check-report)
