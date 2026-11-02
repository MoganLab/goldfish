;; pi-tui-send-event!
;; 向 TUI 事件通道发送异步事件，自带超时控制。
;;
;; 语法
;; ----
;; (pi-tui-send-event! ch ev)
;;
;; 参数
;; ----
;; ch : chan?
;; 事件通道。
;; ev : any
;; 待发送的事件对象。
;;
;; 返回值
;; ----
;; boolean?
;; 发送成功返回 #t，通道满或异常返回 #f。

(import (scheme base)
  (liii check)
  (liii go)
  (liii pi code modes interactive worker-emit)
) ;import

(check-set-mode! 'report-failed)

;; 1. 成功向通道发送事件
(let ((ch (make-chan 4)))
  (check-true (pi-tui-send-event! ch '(test_event 123)))
  (check (chan-try-recv! ch #f) => '(test_event 123))
) ;let

;; 2. 向非通道发送捕获异常并返回 #f
(check-false (pi-tui-send-event! #f '(test_event 123)))

(check-report)
