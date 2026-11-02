;; screen-buffer
;; 获取主屏幕驱动内部持有的行缓冲区实例。
;;
;; 语法
;; ----
;; (screen-buffer screen)
;;
;; 参数
;; ----
;; screen : main-screen?
;; 主屏幕驱动实例。
;;
;; 返回值
;; ----
;; screen-buffer?
;; 主屏幕内部持有的行缓冲区实例。

(import (scheme base)
  (liii check)
  (liii tui terminal)
  (liii tui main-screen buffer)
  (liii tui main-screen driver)
) ;import

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 20 :height 5))
       (screen (make-main-screen vt))
      ) ;
  (check-true (screen-buffer? (screen-buffer screen)))
  ;; 多次获取返回同一个缓冲区实例
  (check (eq? (screen-buffer screen) (screen-buffer screen)) => #t)
) ;let*

(check-catch 'type-error (screen-buffer "not-a-screen"))

(check-report)
