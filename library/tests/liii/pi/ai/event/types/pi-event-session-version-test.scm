;; pi-event-session-version
;; 获取会话启动事件 record 的 version 字段。
;;
;; 语法
;; ----
;; (pi-event-session-version obj)
;;
;; 参数
;; ----
;; obj : any?
;; 会话启动事件 record 实例。
;;
;; 返回值
;; ----
;; integer?
;; version 字段的值。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-event-session 3 "session-uuid-1" "2026-01-01T00:00:00Z" "/tmp/work")
       ) ;obj-a
       (obj-b (%make-pi-event-session 3 "session-uuid-2" "2026-01-02T00:00:00Z" "/home/user")
       ) ;obj-b
      ) ;
  (check (pi-event-session-version obj-a) => 3)
  (check (pi-event-session-version obj-b) => 3)
) ;let*

(check-report)
