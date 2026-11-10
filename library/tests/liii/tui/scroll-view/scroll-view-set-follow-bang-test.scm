;; scroll-view-set-follow!
;; 设置视口的跟随模式开关。
;;
;; 语法
;; ----
;; (scroll-view-set-follow! sv follow?)
;;
;; 参数
;; ----
;; sv : scroll-view?
;; 滚动视口实例。
;;
;; follow? : boolean?
;; 是否开启跟随模式。
;;
;; 返回值
;; ----
;; #t
;; 设置成功返回 #t。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui scroll-view)
) ;import

(check-set-mode! 'report-failed)

(let ((sv (make-scroll-view (make-text "test"))))
  (check (scroll-view-set-follow! sv #f) => #t)
  (check-false (scroll-view-follow? sv))
  (check (scroll-view-set-follow! sv #t) => #t)
  (check-true (scroll-view-follow? sv))
) ;let

(check-report)
