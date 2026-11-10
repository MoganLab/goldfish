;; virtual-terminal?
;; 判断给定对象是否为虚拟终端实例。
;;
;; 语法
;; ----
;; (virtual-terminal? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为虚拟终端实例返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui terminal))

(check-set-mode! 'report-failed)

(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (check-true (virtual-terminal? vt))
  (check-true (terminal? vt))
) ;let

(let* ((p (open-output-string)) (st (make-standard-terminal :output-port p)))
  (check-false (virtual-terminal? st))
) ;let*

(check-false (virtual-terminal? "not-a-terminal"))
(check-false (virtual-terminal? 123))

(check-report)
