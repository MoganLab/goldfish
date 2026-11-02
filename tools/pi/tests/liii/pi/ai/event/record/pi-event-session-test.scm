;; pi-event-session
;; 构造 session header 事件对象。
;;
;; 语法
;; ----
;; (pi-event-session cwd)
;; (pi-event-session cwd id)
;; (pi-event-session cwd id timestamp)
;;
;; 参数
;; ----
;; cwd : string?
;; 工作目录路径。
;;
;; id : string?
;; 可选，会话 UUID。
;;
;; timestamp : string?
;; 可选，ISO 8601 时间戳字符串。
;;
;; 返回值
;; ----
;; pi-event-session?
;; 会话头事件 record。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(let ((ev (pi-event-session "/tmp/cwd")))
  (check-true (pi-event-session? ev))
  (check (pi-event-session-type ev) => "session")
  (check (pi-event-session-version ev) => 3)
  (check (pi-event-session-cwd ev) => "/tmp/cwd")
  (check-true (string? (pi-event-session-id ev)))
  (check-true (string? (pi-event-session-timestamp ev)))
) ;let

(check-report)
