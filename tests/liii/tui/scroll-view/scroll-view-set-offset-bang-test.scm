;; scroll-view-set-offset!
;; 显式设置视口的行偏移量。
;;
;; 语法
;; ----
;; (scroll-view-set-offset! sv offset)
;;
;; 参数
;; ----
;; sv : scroll-view?
;; 滚动视口实例。
;;
;; offset : integer?
;; 目标偏移行数。
;;
;; 返回值
;; ----
;; integer?
;; 设置后的偏移量。

(import (scheme base) (liii check) (liii tui component) (liii tui scroll-view))

(check-set-mode! 'report-failed)

(let ((sv (make-scroll-view (make-text "hello"))))
  (check (scroll-view-set-offset! sv 2) => 2)
  (check (scroll-view-offset sv) => 2)
) ;let

(let ((sv (make-scroll-view (make-text "x"))))
  (check-catch 'type-error (scroll-view-set-offset! sv "2"))
) ;let

(check-report)
