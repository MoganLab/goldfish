;; fs-handler-on-deleted
;; 读取事件处理器的 "deleted" 回调槽。
;;
;; 语法
;; ----
;; (fs-handler-on-deleted handler)
;;
;; 参数
;; ----
;; handler : fs-event-handler?
;; 事件处理器。
;;
;; 返回值
;; ----
;; procedure 或 #f
;; "deleted" 事件回调；未注册时为 #f。

(import (scheme base) (liii check) (liii fs-watch))

(check-set-mode! 'report-failed)

(let ((h (make-fs-event-handler :on-deleted (lambda (ctx ev) 'del))))
  (check-true (procedure? (fs-handler-on-deleted h)))
  (check ((fs-handler-on-deleted h) 'ctx (make-fs-event 'deleted "/x" #f)) => 'del)
) ;let

(check-false (fs-handler-on-deleted (make-fs-event-handler)))

(check-report)
