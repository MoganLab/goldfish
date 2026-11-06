;; fs-handler-on-modified
;; 读取事件处理器的 "modified" 回调槽。
;;
;; 语法
;; ----
;; (fs-handler-on-modified handler)
;;
;; 参数
;; ----
;; handler : fs-event-handler?
;; 事件处理器。
;;
;; 返回值
;; ----
;; procedure 或 #f
;; "modified" 事件回调；未注册时为 #f。

(import (scheme base) (liii check) (liii fs-watch))

(check-set-mode! 'report-failed)

(let ((h (make-fs-event-handler :on-modified (lambda (ctx ev) 'mod))))
  (check-true (procedure? (fs-handler-on-modified h)))
  (check ((fs-handler-on-modified h) 'ctx (make-fs-event 'modified "/x" #f)) => 'mod)
) ;let

(check-false (fs-handler-on-modified (make-fs-event-handler)))

(check-report)
