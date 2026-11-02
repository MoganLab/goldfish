;; %inject-steering
;; 将排队的引导文本转换为 user 消息追加到会话中，并派发对应的事件通知。
;;
;; 语法
;; ----
;; (%inject-steering msgs steering emit)
;;
;; 参数
;; ----
;; msgs : list?，原有的会话消息列表。
;; steering : list?，待注入的引导文本列表（字符串列表）。
;; emit : (or procedure? #f)，事件派发回调过程。
;;
;; 返回值
;; ----
;; list?，追加了新 user 消息后的消息列表。

(import (scheme base)
  (liii check)
  (liii pi ai message)
  (liii pi agent loop-queue)
) ;import

(check-set-mode! 'report-failed)

;; 1. 注入单条 steering 消息，触发 message_start 与 message_end 事件
(let* ((evs '())
       (mock-emit
         (lambda (ev-type msg) (set! evs (cons (cons ev-type msg) evs)))
       ) ;mock-emit
       (orig-msgs '())
       (res (%inject-steering orig-msgs '("focus on task B") mock-emit))
      ) ;
  (check (length res) => 1)
  (check-true (pi-user-message? (car res)))
  (check (pi-user-message-content (car res)) => "focus on task B")
  (check (length evs) => 2)
  (check (car (reverse evs)) => (cons 'message_start (car res)))
  (check (cadr (reverse evs)) => (cons 'message_end (car res)))
) ;let*

;; 2. emit 为 #f 时正常追加消息且不抛错
(let* ((orig (list (pi-user-message "hello")))
       (res (%inject-steering orig '("more details") #f))
      ) ;
  (check (length res) => 2)
  (check (pi-user-message-content (car res)) => "hello")
  (check (pi-user-message-content (cadr res)) => "more details")
) ;let*

;; 3. steering 为空列表时返回原消息列表
(let ((orig (list (pi-user-message "initial"))))
  (check (%inject-steering orig '() #f) => orig)
) ;let

(check-report)
