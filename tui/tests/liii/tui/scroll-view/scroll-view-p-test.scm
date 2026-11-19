;; scroll-view?
;; 判断给定对象是否为滚动视口实例。
;;
;; 语法
;; ----
;; (scroll-view? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为滚动视口实例返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui scroll-view)
) ;import

(check-set-mode! 'report-failed)

(let ((sv (make-scroll-view (make-text "test"))))
  (check-true (scroll-view? sv))
) ;let

(check-false (scroll-view? "not-a-sv"))
(check-false (scroll-view? 123))
(check-false (scroll-view? '()))

(check-report)
