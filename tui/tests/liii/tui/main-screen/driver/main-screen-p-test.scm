;; main-screen?
;; 判断给定对象是否为主屏幕驱动实例。
;;
;; 语法
;; ----
;; (main-screen? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为主屏幕驱动实例返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui terminal)
  (liii tui main-screen driver)
) ;import

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 20 :height 5))
       (screen (make-main-screen vt))
      ) ;let*
  (check-true (main-screen? screen))
) ;let*

(check-false (main-screen? "not-a-screen"))
(check-false (main-screen? 123))

(check-report)
