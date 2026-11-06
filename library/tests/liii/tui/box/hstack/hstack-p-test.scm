;; hstack?
;; 判断给定对象是否为水平分列（hstack）组件。
;;
;; 语法
;; ----
;; (hstack? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为水平分列组件返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui box))

(check-set-mode! 'report-failed)

(let ((h (make-hstack "a" "b")))
  (check-true (hstack? h))
) ;let

(check-false (hstack? "not-an-hstack"))
(check-false (hstack? 123))

(check-report)
