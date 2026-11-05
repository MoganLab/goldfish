;; %make-pi-user-message
;; 构造用户消息 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-user-message content timestamp)
;;
;; 参数
;; ----
;; content : any?
;; 消息正文，字符串或内容块列表
;;
;; timestamp : number?
;; 消息创建时间戳（毫秒）
;;
;; 返回值
;; ----
;; pi-user-message?
;; 用户消息 record 实例。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-user-message "你好" 1000)))
  (check-true (pi-user-message? obj))
  (check (pi-user-message-content obj) => "你好")
  (check (pi-user-message-timestamp obj) => 1000)
) ;let

(check-report)
