(import (liii check) (liii pi ai event) (liii pi ai message))

(check-set-mode! 'report-failed)

;; pi-user-message
;; 构造 user 角色消息 record 对象。
(let* ((msg (pi-user-message "hello goldfish")))
  ;; record 谓词与访问器
  (check-true (pi-user-message? msg))
  (check-true (pi-message? msg))
  (check (pi-message-role msg) => "user")
  (check (pi-user-message-content msg) => "hello goldfish")
  (check-true (number? (pi-user-message-timestamp msg)))

  ;; 指定 timestamp 构造
  (let ((msg2 (pi-user-message "foo" 123456789)))
    (check (pi-user-message-timestamp msg2) => 123456789))

  ;; 兼容 alist 访问
  (check (cdr (assoc "role" msg)) => "user")
  (check (cdr (assoc 'role msg)) => "user")
  (check (cdr (assoc "content" msg)) => "hello goldfish")
  (check (cdr (assoc 'content msg)) => "hello goldfish")
  (check (cdr (assoc "timestamp" msg)) => (pi-user-message-timestamp msg))
  (check (cdr (assoc 'timestamp msg)) => (pi-user-message-timestamp msg)))

(check-report)
