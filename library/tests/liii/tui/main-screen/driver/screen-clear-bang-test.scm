;; screen-clear!
;; 清空终端表面并重置主屏幕的帧行缓存。
;;
;; 语法
;; ----
;; (screen-clear! screen)
;;
;; 参数
;; ----
;; screen : main-screen?
;; 主屏幕驱动实例。
;;
;; 返回值
;; ----
;; #t
;; 清屏完成返回 #t。

(import (scheme base)
  (liii check)
  (liii tui terminal)
  (liii tui main-screen driver)
) ;import

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 10 :height 3))
       (screen (make-main-screen vt))
      ) ;
  (screen-render! screen '("x" "y" "z"))
  (check (screen-clear! screen) => #t)
  (check (screen-lines screen) => '())
  (check (virtual-terminal-screen vt)
    =>
    '("          " "          " "          ")
  ) ;check
) ;let*

(check-catch 'type-error (screen-clear! "not-a-screen"))

(check-report)
