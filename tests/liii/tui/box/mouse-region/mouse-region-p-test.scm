;; mouse-region?
;; 判断给定对象是否为 mouse-region 包装组件。
;;
;; 语法
;; ----
;; (mouse-region? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为 mouse-region 包装组件返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui box mouse-region)
) ;import

(check-set-mode! 'report-failed)

(let ((mr (make-mouse-region (make-text "ok") (lambda (ev) #f))))
  (check-true (mouse-region? mr))
) ;let

(check-false (mouse-region? "not-a-mr"))
(check-false (mouse-region? (make-text "plain")))
(check-false (mouse-region? 123))

(check-report)
