;; alt-screen?
;; 判断给定对象是否为备用屏驱动实例。
;;
;; 语法
;; ----
;; (alt-screen? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为备用屏驱动实例返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui terminal)
  (liii tui alt-screen)
) ;import

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 20 :height 5))
       (screen (make-alt-screen vt))
      ) ;let*
  (check-true (alt-screen? screen))
) ;let*

(check-false (alt-screen? "not-a-screen"))
(check-false (alt-screen? 123))
(check-false (alt-screen? '()))

(check-report)
