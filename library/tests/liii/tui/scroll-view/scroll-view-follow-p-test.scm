;; scroll-view-follow?
;; 判断视口是否处于跟随内容末尾（follow）的模式。
;;
;; 语法
;; ----
;; (scroll-view-follow? sv)
;;
;; 参数
;; ----
;; sv : scroll-view?
;; 滚动视口实例。
;;
;; 返回值
;; ----
;; boolean?
;; 若处于自动跟随模式返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui component) (liii tui scroll-view))

(check-set-mode! 'report-failed)

(let ((sv (make-scroll-view (make-text "test"))))
  (check-true (scroll-view-follow? sv))
) ;let

(check-catch 'type-error (scroll-view-follow? "not-a-sv"))

(check-report)
