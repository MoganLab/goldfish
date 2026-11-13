;; %pi-app-send-abort-signals!
;; 发射中止信号：busy 时向 abort-chan 发送 'abort，! 命令运行时向 bash-abort-chan 发送 'abort。
;;
;; 语法
;; ----
;; (%pi-app-send-abort-signals! app q)
;;
;; 参数
;; ----
;; app : pi-app?，交互式应用实例。
;; q : pi-message-queue?，应用的消息调度队列。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii go)
  (liii tui)
  (liii pi agent queue)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive app-queue)
) ;import

(check-set-mode! 'report-failed)

;; 1. busy 时向 abort-chan 发送信号
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (pi-queue-set-busy! q)
  (%pi-app-send-abort-signals! app q)
  (check (chan-try-recv! (pi-app-abort-chan app) #f) => 'abort)
) ;let*

;; 2. idle 且无运行中 bash 命令时不发送任何信号
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (%pi-app-send-abort-signals! app q)
  (check (chan-try-recv! (pi-app-abort-chan app) #f) => #f)
  (check (chan-try-recv! (pi-app-bash-abort-chan app) #f) => #f)
) ;let*

(check-report)
