;; pi-bash-send-event!
;; 向事件通道发送 Bash 完成事件。
;;
;; 语法
;; ----
;; (pi-bash-send-event! ch ev)
;;
;; 参数
;; ----
;; ch : chan?
;; 事件投递通道。
;; ev : any
;; 待发送事件数据。
;;
;; 返回值
;; ----
;; boolean?
;; 发送成功返回 #t，异常返回 #f。

(import (scheme base)
  (liii check)
  (liii go)
  (liii pi code modes interactive bash)
) ;import

(check-set-mode! 'report-failed)

;; 1. 成功发送事件
(let ((ch (make-chan 4)))
  (check-true (pi-bash-send-event! ch '(bash_complete 1 "output" 0)))
  (check (chan-try-recv! ch #f) => '(bash_complete 1 "output" 0))
) ;let

;; 2. 向非法通道发送安全返回 #f
(check-false (pi-bash-send-event! #f 'test))

(check-report)
