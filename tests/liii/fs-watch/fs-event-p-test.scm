;; fs-event?
;; 判断对象是否为文件系统事件记录。
;;
;; 语法
;; ----
;; (fs-event? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ----
;; boolean
;; obj 是 <fs-event> 记录时返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; macOS kqueue 后端只产生目录级 'modified 事件（path 为监听目录本身）；
;; Windows/Linux 提供具体文件的事件细分。

(import (scheme base) (liii check) (liii fs-watch))

(check-set-mode! 'report-failed)

(check-true (fs-event? (make-fs-event 'modified "/x/HEAD" #f)))
(check-true (fs-event? (make-fs-event 'moved "/a/old" "/a/new")))

(check-false (fs-event? "not-event"))
(check-false (fs-event? '()))
(check-false (fs-event? #f))
(check-false (fs-event? 42))

(check-report)
