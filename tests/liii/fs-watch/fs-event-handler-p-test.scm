;; fs-event-handler?
;; 判断对象是否为文件系统事件处理器记录。
;;
;; 语法
;; ----
;; (fs-event-handler? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ----
;; boolean
;; obj 是 <fs-event-handler> 记录时返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii fs-watch))

(check-set-mode! 'report-failed)

(check-true (fs-event-handler? (make-fs-event-handler)))
(check-true (fs-event-handler? (make-fs-event-handler :on-created (lambda (ctx ev) #t))))

(check-false (fs-event-handler? "not-handler"))
(check-false (fs-event-handler? '()))
(check-false (fs-event-handler? #f))
(check-false (fs-event-handler? (make-fs-event 'modified "/x" #f)))

(check-report)
