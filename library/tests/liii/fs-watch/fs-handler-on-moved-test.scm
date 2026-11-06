;; fs-handler-on-moved
;; 读取事件处理器的 "moved" 回调槽。
;;
;; 语法
;; ----
;; (fs-handler-on-moved handler)
;;
;; 参数
;; ----
;; handler : fs-event-handler?
;; 事件处理器。
;;
;; 返回值
;; ----
;; procedure 或 #f
;; "moved" 事件回调（事件带 dest-path 目标路径）；未注册时为 #f。

(import (scheme base) (liii check) (liii fs-watch))

(check-set-mode! 'report-failed)

(let ((h (make-fs-event-handler :on-moved (lambda (ctx ev) 'mov))))
  (check-true (procedure? (fs-handler-on-moved h)))
  (check ((fs-handler-on-moved h) 'ctx (make-fs-event 'moved "/a/old" "/a/new")) => 'mov)
) ;let

(check-false (fs-handler-on-moved (make-fs-event-handler)))

(check-report)
