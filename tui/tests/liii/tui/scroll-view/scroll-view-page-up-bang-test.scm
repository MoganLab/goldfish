;; scroll-view-page-up!
;; 将视口向上滚动一页（视口高度减 1 行）。
;;
;; 语法
;; ----
;; (scroll-view-page-up! sv)
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

(import (scheme base) (liii check) (liii tui component) (liii tui scroll-view))

(check-set-mode! 'report-failed)

(define (make-lines-component lines)
  (make-component (lambda (comp width) lines))
) ;define

(let ((sv (make-scroll-view (make-lines-component '("l0" "l1" "l2" "l3" "l4"
                                                    "l5"))))
     ) ;
  (scroll-view-render-lines sv 20 2)
  (check (scroll-view-page-up! sv) => 2)
  (check (scroll-view-render-lines sv 20 2) => '("l2" "l3"))
) ;let

(check-catch 'type-error (scroll-view-page-up! "not-a-sv"))

(check-report)
