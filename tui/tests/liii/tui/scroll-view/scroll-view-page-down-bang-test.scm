;; scroll-view-page-down!
;; 将视口向下滚动一页（视口高度减 1 行）。
;;
;; 语法
;; ----
;; (scroll-view-page-down! sv)
;;
;; 参数
;; ----
;; sv : scroll-view?
;; 滚动视口实例。
;;
;; 返回值
;; ----
;; integer?
;; 滚动后的实际行偏移量。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui scroll-view)
) ;import

(check-set-mode! 'report-failed)

(define (make-lines-component lines)
  (make-component (lambda (comp width) lines))
) ;define

(let ((sv (make-scroll-view (make-lines-component '("l0" "l1" "l2" "l3" "l4" "l5")))))
  (scroll-view-render-lines sv 20 2)
  (scroll-view-page-up! sv)
  (check (scroll-view-page-down! sv) => 4)
  (check (scroll-view-render-lines sv 20 2) => '("l4" "l5"))
) ;let

(check-catch 'type-error (scroll-view-page-down! "not-a-sv"))

(check-report)
