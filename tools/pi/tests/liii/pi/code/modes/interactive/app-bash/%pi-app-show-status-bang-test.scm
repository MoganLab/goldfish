;; %pi-app-show-status!
;; 在应用聊天区显示或原地更新状态通知卡片。
;;
;; 语法
;; ----
;; (%pi-app-show-status! app message)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; message : string?
;; 要显示的状态消息。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive app-bash)
) ;import

(check-set-mode! 'report-failed)

;; 1. 首次调用在 chat 中添加状态卡片
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (%pi-app-show-status! app "Hello status")
  (check (length (container-children (%pi-app-chat app))) => 1)

  ;; 2. 再次调用时原地复用更新现有状态卡片，不新增组件
  (%pi-app-show-status! app "Updated status")
  (check (length (container-children (%pi-app-chat app))) => 1)
) ;let*

(check-report)
