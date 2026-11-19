;; standard-terminal?
;; 判断给定对象是否为标准（真实）终端实例。
;;
;; 语法
;; ----
;; (standard-terminal? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为标准终端实例返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui terminal))

(check-set-mode! 'report-failed)

(let* ((p (open-output-string)) (st (make-standard-terminal :output-port p)))
  (check-true (standard-terminal? st))
  (check-false (virtual-terminal? st))
) ;let*

(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (check-false (standard-terminal? vt))
) ;let

(check-false (standard-terminal? "not-a-terminal"))
(check-false (standard-terminal? 123))

(check-report)
