;; %queue-before-stop
;; 在单轮对话自然停止前检查指定队列，若有排队消息则注入并返回新列表。
;;
;; 语法
;; ----
;; (%queue-before-stop msgs config emit key)
;;
;; 参数
;; ----
;; msgs : list?，当前会话消息列表。
;; config : list?，包含轮询回调的配置 alist。
;; emit : (or procedure? #f)，事件派发过程。
;; key : any?，配置项键名（如 'get-steering-messages 或 'get-follow-up-messages）。
;;
;; 返回值
;; ----
;; (or list? #f)，有消息时返回注入后的新消息列表，无排队消息则返回 #f。

(import (scheme base)
  (liii check)
  (liii pi ai message)
  (liii pi agent loop-queue)
) ;import

(check-set-mode! 'report-failed)

;; 1. 对应队列有排队消息：注入并返回新列表
(let* ((cfg
         (list (cons 'get-follow-up-messages (lambda () '("follow up task 1"))))
       ) ;cfg
       (msgs (list (pi-user-message "first")))
       (res (%queue-before-stop msgs cfg #f 'get-follow-up-messages))
      ) ;
  (check-true (pair? res))
  (check (length res) => 2)
  (check (pi-user-message-content (cadr res)) => "follow up task 1")
) ;let*

;; 2. 对应队列为空：返回 #f
(let* ((cfg (list (cons 'get-follow-up-messages (lambda () '()))))
       (msgs (list (pi-user-message "first")))
       (res (%queue-before-stop msgs cfg #f 'get-follow-up-messages))
      ) ;
  (check res => #f)
) ;let*

;; 3. 配置中未找到对应队列的回调：返回 #f
(let* ((msgs (list (pi-user-message "first")))
       (res (%queue-before-stop msgs '() #f 'get-follow-up-messages))
      ) ;
  (check res => #f)
) ;let*

(check-report)
