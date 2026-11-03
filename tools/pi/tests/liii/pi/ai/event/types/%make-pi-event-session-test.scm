;; %make-pi-event-session
;; 构造会话启动事件 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-event-session version id timestamp cwd)
;;
;; 参数
;; ----
;; version : integer?
;; 事件协议版本号
;;
;; id : string?
;; 会话 UUID
;;
;; timestamp : string?
;; ISO 8601 时间戳字符串
;;
;; cwd : string?
;; 工作目录路径
;;
;; 返回值
;; ----
;; pi-event-session?
;; 会话启动事件 record 实例。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-session 3 "session-uuid-1" "2026-01-01T00:00:00Z" "/tmp/work")
      ) ;obj
     ) ;
  (check-true (pi-event-session? obj))
  (check (pi-event-session-version obj) => 3)
  (check (pi-event-session-id obj) => "session-uuid-1")
  (check (pi-event-session-timestamp obj) => "2026-01-01T00:00:00Z")
  (check (pi-event-session-cwd obj) => "/tmp/work")
) ;let

(check-report)
