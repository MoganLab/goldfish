;; scroll-view-to-top!
;; 将视口直接滚动到最顶部（偏移量为 0），退出 follow 模式。
;;
;; 语法
;; ----
;; (scroll-view-to-top! sv)
;;
;; 参数
;; ----
;; sv : scroll-view?
;; 滚动视口实例。
;;
;; 返回值
;; ----
;; integer?
;; 滚动后的行偏移量（始终为 0）。

(import (scheme base) (liii check) (liii tui component) (liii tui scroll-view))

(check-set-mode! 'report-failed)

(define (make-lines-component lines)
  (make-component (lambda (comp width) lines))
) ;define

(let ((sv (make-scroll-view (make-lines-component '("l0" "l1" "l2" "l3" "l4")))))
  (scroll-view-render-lines sv 20 2)
  (check (scroll-view-to-top! sv) => 0)
  (check-false (scroll-view-follow? sv))
  (check (scroll-view-render-lines sv 20 2) => '("l0" "l1"))
) ;let

(check-catch 'type-error (scroll-view-to-top! "not-a-sv"))

(check-report)
