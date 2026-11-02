;; screen-lines
;; 获取主屏幕当前帧的文本行列表。
;;
;; 语法
;; ----
;; (screen-lines screen)
;;
;; 参数
;; ----
;; screen : main-screen?
;; 主屏幕驱动实例。
;;
;; 返回值
;; ----
;; list?
;; 当前帧的文本行列表。

(import (scheme base)
  (liii check)
  (liii tui terminal)
  (liii tui main-screen driver)
) ;import

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 20 :height 5))
       (screen (make-main-screen vt))
      ) ;
  (check (screen-lines screen) => '())
  (screen-render! screen '("hello" "world"))
  (check (screen-lines screen) => '("hello" "world"))
) ;let*

(check-catch 'type-error (screen-lines "not-a-screen"))

(check-report)
