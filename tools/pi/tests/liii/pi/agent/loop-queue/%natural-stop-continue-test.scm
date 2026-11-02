;; %natural-stop-continue
;; 在 Agent 自然停止前检测 steering 引导队列与 follow-up 跟进队列，决定是否开启新一轮循环。
;;
;; 语法
;; ----
;; (%natural-stop-continue msgs config emit)
;;
;; 参数
;; ----
;; msgs : list?，当前会话消息列表。
;; config : list?，包含 get-steering-messages 和 get-follow-up-messages 回调的配置 alist。
;; emit : (or procedure? #f)，事件派发过程。
;;
;; 返回值
;; ----
;; (or list? #f)，任一队列有消息时注入并返回新消息列表（同时派发 turn_start 事件），均为空则返回 #f。

(import (scheme base)
  (liii check)
  (liii pi ai message)
  (liii pi agent loop-queue)
) ;import

(check-set-mode! 'report-failed)

;; 1. steering 队列有消息：优先注入 steering，并派发 turn_start
(let* ((evs '())
       (emit (lambda (ev-type . args) (set! evs (cons ev-type evs))))
       (cfg
         (list (cons 'get-steering-messages (lambda () '("steering msg")))
           (cons 'get-follow-up-messages (lambda () '("follow-up msg")))
         ) ;list
       ) ;cfg
       (msgs (list (pi-user-message "init")))
       (res (%natural-stop-continue msgs cfg emit))
      ) ;
  (check-true (pair? res))
  (check (length res) => 2)
  (check (pi-user-message-content (cadr res)) => "steering msg")
  (check-true (pair? (member 'turn_start evs)))
) ;let*

;; 2. steering 为空但 follow-up 有消息：注入 follow-up 并派发 turn_start
(let* ((evs '())
       (emit (lambda (ev-type . args) (set! evs (cons ev-type evs))))
       (cfg
         (list (cons 'get-steering-messages (lambda () '()))
           (cons 'get-follow-up-messages (lambda () '("follow-up msg")))
         ) ;list
       ) ;cfg
       (msgs (list (pi-user-message "init")))
       (res (%natural-stop-continue msgs cfg emit))
      ) ;
  (check-true (pair? res))
  (check (length res) => 2)
  (check (pi-user-message-content (cadr res)) => "follow-up msg")
  (check-true (pair? (member 'turn_start evs)))
) ;let*

;; 3. 两者均为空：返回 #f，不派发 turn_start
(let* ((evs '())
       (emit (lambda (ev-type . args) (set! evs (cons ev-type evs))))
       (cfg (list (cons 'get-steering-messages (lambda () '()))
              (cons 'get-follow-up-messages (lambda () '()))
            ) ;list
       ) ;cfg
       (msgs (list (pi-user-message "init")))
       (res (%natural-stop-continue msgs cfg emit))
      ) ;
  (check res => #f)
  (check-false (member 'turn_start evs))
) ;let*

(check-report)
