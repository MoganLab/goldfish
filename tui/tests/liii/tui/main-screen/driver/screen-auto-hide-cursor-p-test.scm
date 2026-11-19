;; screen-auto-hide-cursor?
;; 查询主屏幕是否启用了自动隐藏光标。
;;
;; 语法
;; ----
;; (screen-auto-hide-cursor? screen)
;;
;; 参数
;; ----
;; screen : main-screen?
;; 主屏幕驱动实例。
;;
;; 返回值
;; ----
;; boolean?
;; 若启用自动隐藏光标返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui terminal)
  (liii tui main-screen driver)
) ;import

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 20 :height 5))
       (screen (make-main-screen vt))
      ) ;
  (check-true (screen-auto-hide-cursor? screen))
  (screen-auto-hide-cursor-set! screen #f)
  (check-false (screen-auto-hide-cursor? screen))
  (screen-auto-hide-cursor-set! screen #t)
  (check-true (screen-auto-hide-cursor? screen))
) ;let*

(check-catch 'type-error (screen-auto-hide-cursor? "not-a-screen"))

(check-report)
