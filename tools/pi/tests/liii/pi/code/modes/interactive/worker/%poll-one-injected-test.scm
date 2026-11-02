;; %poll-one-injected
;; 从注入通道轮询单条消息并投递 user_message 事件到事件通道。
;;
;; 语法
;; ----
;; (%poll-one-injected chan event-chan)
;;
;; 参数
;; ----
;; chan : chan?
;; steering 或 follow-up 消息通道。
;; event-chan : chan?
;; 事件投递通道。
;;
;; 返回值
;; ----
;; list?
;; 包含轮询到消息的列表，通道为空或无消息时返回空列表 '()。

(import (scheme base)
  (liii check)
  (liii go)
  (liii pi code modes interactive worker)
) ;import

(check-set-mode! 'report-failed)

;; 1. 通道有消息时收取并投递事件
(let* ((in-ch (make-chan 4)) (ev-ch (make-chan 4)))
  (chan-send! in-ch "injected text" 100)
  (check (%poll-one-injected in-ch ev-ch) => '("injected text"))
  (check (chan-try-recv! ev-ch #f) => '(user_message "injected text"))
) ;let*

;; 2. 通道无消息时返回空列表
(let* ((in-ch (make-chan 4)) (ev-ch (make-chan 4)))
  (check (%poll-one-injected in-ch ev-ch) => '())
  (check (chan-try-recv! ev-ch #f) => #f)
) ;let*

(check-report)
