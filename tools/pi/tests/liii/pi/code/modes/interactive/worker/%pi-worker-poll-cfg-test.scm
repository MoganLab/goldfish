;; %pi-worker-poll-cfg
;; 注入 steering 与 follow-up 轮询回调到 Worker 配置中。
;;
;; 语法
;; ----
;; (%pi-worker-poll-cfg cfg steering-chan follow-up-chan event-chan)
;;
;; 参数
;; ----
;; cfg : list?
;; 原始配置列表。
;; steering-chan : chan?
;; steering 消息通道。
;; follow-up-chan : chan?
;; follow-up 消息通道。
;; event-chan : chan?
;; 事件投递通道。
;;
;; 返回值
;; ----
;; list?
;; 注入了 get-steering-messages 与 get-follow-up-messages 回调的新配置。

(import (scheme base)
  (liii check)
  (liii go)
  (liii pi code modes interactive worker)
) ;import

(check-set-mode! 'report-failed)

;; 1. 注入的回调键存在且为过程
(let* ((st-ch (make-chan 4))
       (fu-ch (make-chan 4))
       (ev-ch (make-chan 4))
       (cfg (%pi-worker-poll-cfg '((api-key . "key")) st-ch fu-ch ev-ch))
      ) ;
  (check-true (procedure? (cdr (assoc 'get-steering-messages cfg))))
  (check-true (procedure? (cdr (assoc 'get-follow-up-messages cfg))))
) ;let*

(check-report)
