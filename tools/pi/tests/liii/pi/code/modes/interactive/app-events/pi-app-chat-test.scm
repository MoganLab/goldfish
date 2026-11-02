;; pi-app-chat
;; 获取应用当前包含的对话消息卡片列表。
;;
;; 语法
;; ----
;; (pi-app-chat app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; list
;; 当前会话中已添加的消息卡片组件列表。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证获取聊天卡片列表
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check (pi-app-chat app) => '())
  (pi-app-add-user-message! app "msg1")
  (check (length (pi-app-chat app)) => 1)
) ;let

(check-report)
