;; %maybe-inject-steering!
;; 在每轮请求发起前轮询 steering 引导队列，有排队消息则注入会话并触发事件。
;;
;; 语法
;; ----
;; (%maybe-inject-steering! msgs config emit)
;;
;; 参数
;; ----
;; msgs : list?，当前会话消息列表。
;; config : list?，包含 get-steering-messages 回调的配置 alist。
;; emit : (or procedure? #f)，事件派发过程。
;;
;; 返回值
;; ----
;; list?，若有 steering 消息则返回注入后的新消息列表，否则原样返回 msgs。

(import (scheme base)
  (liii check)
  (liii pi ai message)
  (liii pi agent loop-queue)
) ;import

(check-set-mode! 'report-failed)

;; 1. 有排队的 steering 引导消息：注入并返回新列表，触发事件通知
(let* ((evs '())
       (emit
         (lambda (ev-type msg) (set! evs (cons (cons ev-type msg) evs)))
       ) ;emit
       (cfg
         (list (cons 'get-steering-messages (lambda () '("user guidance 1"))))
       ) ;cfg
       (msgs (list (pi-user-message "initial")))
       (res (%maybe-inject-steering! msgs cfg emit))
      ) ;
  (check (length res) => 2)
  (check (pi-user-message-content (cadr res)) => "user guidance 1")
  (check (length evs) => 2)
) ;let*

;; 2. 无 steering 引导消息：原样返回原有 msgs
(let* ((cfg (list (cons 'get-steering-messages (lambda () '()))))
       (msgs (list (pi-user-message "initial")))
       (res (%maybe-inject-steering! msgs cfg #f))
      ) ;
  (check res => msgs)
) ;let*

(check-report)
