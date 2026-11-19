;; scroll-view-render-lines
;; 渲染视口内部可见的文本行。
;;
;; 语法
;; ----
;; (scroll-view-render-lines sv width height)
;;
;; 参数
;; ----
;; sv : scroll-view?
;; 滚动视口实例。
;;
;; width : integer?
;; 视口可用宽度。
;;
;; height : integer?
;; 视口可用高度。
;;
;; 返回值
;; ----
;; list?
;; 视口范围内可见的文本行列表。

(import (scheme base) (liii check) (liii tui component) (liii tui scroll-view))

(check-set-mode! 'report-failed)

(define (make-lines-component lines)
  (make-component (lambda (comp width) lines))
) ;define

(let ((sv (make-scroll-view (make-lines-component '("l0" "l1" "l2" "l3" "l4")))))
  ;; 默认 follow 模式下贴底显示最后 2 行
  (check (scroll-view-render-lines sv 20 2) => '("l3" "l4"))
) ;let

(let ((sv (make-scroll-view (make-text "x"))))
  (check-catch 'type-error (scroll-view-render-lines 123 20 2))
) ;let

(check-report)
