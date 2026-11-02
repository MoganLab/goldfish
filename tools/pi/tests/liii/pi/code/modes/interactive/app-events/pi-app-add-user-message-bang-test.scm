;; pi-app-add-user-message!
;; 向应用对话记录追加用户消息卡片。
;;
;; 语法
;; ----
;; (pi-app-add-user-message! app text)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; text : string
;; 用户消息文本。
;;
;; 返回值
;; ------
;; any
;; 执行追加卡片的副作用。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 追加用户消息后聊天容器卡片数增加
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check (length (pi-app-chat app)) => 0)
  (pi-app-add-user-message! app "test user msg")
  (check (length (pi-app-chat app)) => 1)
) ;let

(check-report)
