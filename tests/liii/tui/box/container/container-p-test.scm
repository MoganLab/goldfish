;; container?
;; 判断给定对象是否为容器组件。
;;
;; 语法
;; ----
;; (container? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为容器组件返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui box))

(check-set-mode! 'report-failed)

(let ((c (make-container)))
  (check-true (container? c))
) ;let

(check-false (container? "not-a-container"))
(check-false (container? 123))

(check-report)
