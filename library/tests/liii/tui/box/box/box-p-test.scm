;; box?
;; 判断给定对象是否为边框容器（box）组件。
;;
;; 语法
;; ----
;; (box? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为边框容器组件返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui box))

(check-set-mode! 'report-failed)

(let ((b (make-box "content")))
  (check-true (box? b))
) ;let

(check-false (box? "not-a-box"))
(check-false (box? 123))
(check-false (box? '()))

(check-report)
