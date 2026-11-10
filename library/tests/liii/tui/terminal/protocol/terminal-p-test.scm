;; terminal?
;; 判断给定对象是否为终端协议记录实例。
;;
;; 语法
;; ----
;; (terminal? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为终端协议记录实例返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui terminal)
) ;import

(check-set-mode! 'report-failed)

(let ((vt (make-virtual-terminal :width 20 :height 5)))
  (check-true (terminal? vt))
) ;let

(check-false (terminal? "not-a-terminal"))
(check-false (terminal? 123))

(check-report)
