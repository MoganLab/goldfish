;; pi-session-message-entry-timestamp
;; 会话条目 record 构造与字段访问函数。
;;
;; 语法
;; ----
;; (pi-session-message-entry-timestamp ...)
;;
;; 参数
;; ----
;; entry : any?
;; 会话条目 record 或构造参数。
;;
;; 返回值
;; ----
;; any?
;; 对应的会话条目 record、谓词布尔值或字段值。

(import (liii check) (liii pi ai message) (liii pi code core session-entry message))
(check-set-mode! 'report-failed)
(let ((e (pi-session-message-entry "m1" "h1" "ts" (pi-user-message "hi" 1))))
  (check-true (pi-session-message-entry? e))
  (check (pi-session-message-entry-id e) => "m1")
  (check (pi-session-message-entry-parent-id e) => "h1")
  (check (pi-session-message-entry-timestamp e) => "ts")
  (check-true (pi-user-message? (pi-session-message-entry-message e)))
) ;let

(check-report)
