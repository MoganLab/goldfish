;; pi-app-add-retry-message!
;; 向应用对话记录追加错误重试提示卡片。
;;
;; 语法
;; ----
;; (pi-app-add-retry-message! app msg)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; msg : string
;; 重试提示文本。
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

;; 1. 追加重试卡片后聊天容器卡片数增加
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check (length (pi-app-chat app)) => 0)
  (pi-app-add-retry-message! app "retry count 1")
  (check (length (pi-app-chat app)) => 1)
) ;let

(check-report)
