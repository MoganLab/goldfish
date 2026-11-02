;; scroll-view-height
;; 获取视口最近一次渲染时记录的有效高度。
;;
;; 语法
;; ----
;; (scroll-view-height sv)
;;
;; 参数
;; ----
;; sv : scroll-view?
;; 滚动视口实例。
;;
;; 返回值
;; ----
;; integer? 或 #f
;; 视口高度；若尚未渲染则返回 #f。

(import (scheme base) (liii check) (liii tui component) (liii tui scroll-view))

(check-set-mode! 'report-failed)

(define (make-lines-component lines)
  (make-component (lambda (comp width) lines))
) ;define

(let ((sv (make-scroll-view (make-lines-component '("l0" "l1")))))
  (check (scroll-view-height sv) => #f)
  (scroll-view-render-lines sv 20 5)
  (check (scroll-view-height sv) => 5)
) ;let

(check-catch 'type-error (scroll-view-height "not-a-sv"))

(check-report)
