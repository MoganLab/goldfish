;; %make-pi-event-message-update
;; 构造消息增量事件 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-event-message-update usage assistant-message-event)
;;
;; 参数
;; ----
;; usage : alist?
;; 累计 token 用量统计
;;
;; assistant-message-event : any?
;; 助手消息的增量事件
;;
;; 返回值
;; ----
;; pi-event-message-update?
;; 消息增量事件 record 实例。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-message-update '((input . 10) (output . 5)) "delta-1")))
  (check-true (pi-event-message-update? obj))
  (check (pi-event-message-update-usage obj) => '((input . 10) (output . 5)))
  (check (pi-event-message-update-assistant-message-event obj) => "delta-1")
) ;let

(check-report)
