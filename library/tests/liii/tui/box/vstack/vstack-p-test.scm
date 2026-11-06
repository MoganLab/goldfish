;; vstack?
;; 判断给定对象是否为垂直堆叠（vstack）组件。
;;
;; 语法
;; ----
;; (vstack? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为垂直堆叠组件返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui box))

(check-set-mode! 'report-failed)

(let ((v (make-vstack "a" "b")))
  (check-true (vstack? v))
) ;let

(check-false (vstack? "not-a-vstack"))
(check-false (vstack? 123))

(check-report)
