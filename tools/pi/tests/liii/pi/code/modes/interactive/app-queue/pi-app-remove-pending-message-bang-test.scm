;; pi-app-remove-pending-message!
;; 从待处理的 steering 或 follow-up 消息列表中移除已消费的消息。
;;
;; 语法
;; ----
;; (pi-app-remove-pending-message! app text)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; text : string?
;; 已被消费的消息文本。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive app-queue)
) ;import

(check-set-mode! 'report-failed)

;; 1. 优先从 steering 队列中移除
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (pi-app-queue-steering! app "task A")
  (check (member "task A" (%pi-app-steering-pending app)) => '("task A"))
  (pi-app-remove-pending-message! app "task A")
  (check (member "task A" (%pi-app-steering-pending app)) => #f)
) ;let*

;; 2. steering 无匹配时从 follow-up 移除
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (pi-app-queue-follow-up! app "task B")
  (check (member "task B" (%pi-app-follow-up-pending app)) => '("task B"))
  (pi-app-remove-pending-message! app "task B")
  (check (member "task B" (%pi-app-follow-up-pending app)) => #f)
) ;let*

(check-report)
