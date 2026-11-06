;; fs-observer?
;; 判断对象是否为文件系统监听 observer。
;;
;; 语法
;; ----
;; (fs-observer? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ----
;; boolean
;; obj 是 <fs-observer> 记录时返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii fs-watch))

(check-set-mode! 'report-failed)

(check-true (fs-observer? (make-fs-observer)))

(check-false (fs-observer? "not-observer"))
(check-false (fs-observer? '()))
(check-false (fs-observer? #f))
(check-false (fs-observer? (make-fs-event-handler)))

(check-report)
