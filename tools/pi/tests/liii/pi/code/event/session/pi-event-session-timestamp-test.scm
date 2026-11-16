;; pi-event-session-timestamp
;; 获取会话启动事件 record 的 timestamp 字段。
;;
;; 语法
;; ----
;; (pi-event-session-timestamp obj)
;;
;; 参数
;; ----
;; obj : any?
;; 会话启动事件 record 实例。
;;
;; 返回值
;; ----
;; string?
;; timestamp 字段的值。

(import (liii check) (liii pi code event))

(check-set-mode! 'report-failed)

(let* ((obj-a (pi-event-session "/tmp/work" "session-uuid-1" "2026-01-01T00:00:00Z" 3)
       ) ;obj-a
       (obj-b (pi-event-session "/home/user" "session-uuid-2" "2026-01-02T00:00:00Z" 3)
       ) ;obj-b
      ) ;
  (check (pi-event-session-timestamp obj-a) => "2026-01-01T00:00:00Z")
  (check (pi-event-session-timestamp obj-b) => "2026-01-02T00:00:00Z")
) ;let*

(check-report)
