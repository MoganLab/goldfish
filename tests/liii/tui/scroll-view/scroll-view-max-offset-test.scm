;; scroll-view-max-offset
;; 计算当前视口允许的最大行偏移量（total-lines - height，最小为 0）。
;;
;; 语法
;; ----
;; (scroll-view-max-offset sv)
;;
;; 参数
;; ----
;; sv : scroll-view?
;; 滚动视口实例。
;;
;; 返回值
;; ----
;; integer?
;; 允许的最大行偏移量。

(import (scheme base) (liii check) (liii tui component) (liii tui scroll-view))

(check-set-mode! 'report-failed)

(define (make-lines-component lines)
  (make-component (lambda (comp width) lines))
) ;define

(let ((sv (make-scroll-view (make-lines-component '("l0" "l1" "l2" "l3" "l4")))))
  (scroll-view-render-lines sv 20 2)
  (check (scroll-view-max-offset sv) => 3)
) ;let

(check-catch 'type-error (scroll-view-max-offset "not-a-sv"))

(check-report)
