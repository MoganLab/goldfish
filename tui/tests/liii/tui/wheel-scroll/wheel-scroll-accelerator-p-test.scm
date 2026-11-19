;; wheel-scroll-accelerator?
;; 判断给定对象是否为鼠标滚轮加速器实例。
;;
;; 语法
;; ----
;; (wheel-scroll-accelerator? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果是滚轮加速器实例返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui wheel-scroll)
) ;import

(check-set-mode! 'report-failed)

(let ((acc (make-wheel-scroll-accelerator)))
  (check-true (wheel-scroll-accelerator? acc))
) ;let

(check-false (wheel-scroll-accelerator? "not-an-acc"))
(check-false (wheel-scroll-accelerator? 123))
(check-false (wheel-scroll-accelerator? '()))

(check-report)
