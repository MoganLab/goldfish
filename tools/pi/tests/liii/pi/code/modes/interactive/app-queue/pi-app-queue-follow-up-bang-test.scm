;; pi-app-queue-follow-up!
;; 将用户输入作为 follow-up 消息排入通道并记录待处理状态。
;;
;; 语法
;; ----
;; (pi-app-queue-follow-up! app text)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; text : string?
;; 用户 follow-up 消息文本。
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

;; 1. 排入 follow-up 消息
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (pi-app-queue-follow-up! app "next please do refactor")
  (check (member "next please do refactor" (%pi-app-follow-up-pending app))
    =>
    '("next please do refactor")
  ) ;check
) ;let*

(check-report)
