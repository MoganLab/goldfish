;; pi-event-session?
;; 判断对象是否为会话启动事件 record 实例。
;;
;; 语法
;; ----
;; (pi-event-session? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于会话启动事件 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai event))

(check-set-mode! 'report-failed)

(let ((obj (pi-event-session "/tmp/work" "session-uuid-1" "2026-01-01T00:00:00Z" 3)
      ) ;obj
     ) ;
  (check-true (pi-event-session? obj))
) ;let
(check-false (pi-event-session? '()))
(check-false (pi-event-session? #f))
(check-false (pi-event-session? "not-a-record"))

(check-report)
