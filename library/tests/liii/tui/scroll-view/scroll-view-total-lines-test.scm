;; scroll-view-total-lines
;; 获取当前视口中内容的总行数（若未缓存则触发计算）。
;;
;; 语法
;; ----
;; (scroll-view-total-lines sv)
;;
;; 参数
;; ----
;; sv : scroll-view?
;; 滚动视口实例。
;;
;; 返回值
;; ----
;; integer?
;; 内容总行数。

(import (scheme base) (liii check) (liii tui component) (liii tui scroll-view))

(check-set-mode! 'report-failed)

(define (make-lines-component lines)
  (make-component (lambda (comp width) lines))
) ;define

(let ((sv (make-scroll-view (make-lines-component '("l0" "l1" "l2" "l3" "l4")))))
  (check (scroll-view-total-lines sv) => 5)
) ;let

(check-catch 'type-error (scroll-view-total-lines "not-a-sv"))

(check-report)
