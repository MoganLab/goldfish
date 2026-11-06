;; fs-handler-on-created
;; 读取事件处理器的 "created" 回调槽。
;;
;; 语法
;; ----
;; (fs-handler-on-created handler)
;;
;; 参数
;; ----
;; handler : fs-event-handler?
;; 事件处理器。
;;
;; 返回值
;; ----
;; procedure 或 #f
;; "created" 事件回调；未注册时为 #f。

(import (scheme base) (liii check) (liii fs-watch))

(check-set-mode! 'report-failed)

(let ((h (make-fs-event-handler :on-created (lambda (ctx ev) 'cre))))
  (check-true (procedure? (fs-handler-on-created h)))
  (check ((fs-handler-on-created h) 'ctx (make-fs-event 'created "/x" #f)) => 'cre)
) ;let

(check-false (fs-handler-on-created (make-fs-event-handler)))

(check-report)
