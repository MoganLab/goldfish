;; pi-app-poll-events!
;; 轮询异步事件通道并分发处理事件以更新应用状态。
;;
;; 语法
;; ----
;; (pi-app-poll-events! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 执行事件消费与界面重绘的副作用。

(import (scheme base)
  (liii check)
  (liii string)
  (liii go)
  (liii tui)
  (liii pi code core session-stats)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report-failed)

;; 1. 执行常规事件轮询过程
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (pi-app-poll-events! app)
  (check-true #t)
) ;let

;; 2. 处理 message_usage 事件：累加 token 并驱动 footer 呈现 CHxx.x%
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (ch (pi-app-event-chan app))
       (usage '((input . 100) (output . 50) (cacheRead . 1900) (cacheWrite . 0)))
      ) ;
  (chan-send! ch (list 'message_usage usage))
  (pi-app-poll-events! app)
  (check (pi-session-token-stats-cache-read (pi-app-tokens app)) => 1900)
  (check (pi-session-token-stats-input (pi-app-tokens app)) => 100)
  (check-true (string-contains (pi-footer-stats (%pi-app-footer app)) "CH95.0%"))
) ;let*

(check-report)
