;; scroll-view-offset
;; 获取视口当前滚动的行偏移量。
;;
;; 语法
;; ----
;; (scroll-view-offset sv)
;;
;; 参数
;; ----
;; sv : scroll-view?
;; 滚动视口实例。
;;
;; 返回值
;; ----
;; integer?
;; 当前行偏移量。

(import (scheme base) (liii check) (liii tui component) (liii tui scroll-view))

(check-set-mode! 'report-failed)

(let ((sv (make-scroll-view (make-text "hello"))))
  (check (scroll-view-offset sv) => 0)
) ;let

(check-catch 'type-error (scroll-view-offset "not-a-sv"))

(check-report)
