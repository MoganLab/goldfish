;; pi-app-add-assistant-text!
;; 向应用对话记录追加助手消息卡片。
;;
;; 语法
;; ----
;; (pi-app-add-assistant-text! app text)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; text : string
;; 助手回复文本。
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

;; 1. 追加助手文本后聊天容器卡片数增加
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check (length (pi-app-chat app)) => 0)
  (pi-app-add-assistant-text! app "test assistant text")
  (check (length (pi-app-chat app)) => 1)
) ;let

(check-report)
