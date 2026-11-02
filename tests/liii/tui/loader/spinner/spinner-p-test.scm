;; spinner?
;; 判断给定对象是否为微调器（spinner）实例。
;;
;; 语法
;; ----
;; (spinner? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为微调器实例返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui loader spinner))

(check-set-mode! 'report-failed)

(let ((s (make-spinner "testing")))
  (check-true (spinner? s))
) ;let

(check-false (spinner? "not-a-spinner"))
(check-false (spinner? 123))
(check-false (spinner? '()))

(check-report)
