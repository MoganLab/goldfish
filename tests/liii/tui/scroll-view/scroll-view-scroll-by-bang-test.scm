;; scroll-view-scroll-by!
;; 按相对行数滚动视口，退出 follow 模式，滚动回底端时自动恢复 follow。
;;
;; 语法
;; ----
;; (scroll-view-scroll-by! sv delta)
;;
;; 参数
;; ----
;; sv : scroll-view?
;; 滚动视口实例。
;;
;; delta : integer?
;; 滚动的行数增量（正数向下，负数向上）。
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

(let ((sv (make-scroll-view (make-lines-component '("l0" "l1" "l2" "l3" "l4")))))
  (scroll-view-render-lines sv 20 2)
  (check (scroll-view-scroll-by! sv -1) => 2)
  (check-false (scroll-view-follow? sv))
  (check (scroll-view-render-lines sv 20 2) => '("l2" "l3"))
  ;; 滚动到底部自动恢复 follow
  (check (scroll-view-scroll-by! sv 1) => 3)
  (check-true (scroll-view-follow? sv))
) ;let

(let ((sv (make-scroll-view (make-text "x"))))
  (check-catch 'type-error (scroll-view-scroll-by! sv "1"))
) ;let

(check-report)
