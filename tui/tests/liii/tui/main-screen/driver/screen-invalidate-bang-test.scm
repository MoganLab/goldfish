;; screen-invalidate!
;; 使主屏幕内部行缓存失效，强制下一次渲染进行全量重绘。
;;
;; 语法
;; ----
;; (screen-invalidate! screen)
;;
;; 参数
;; ----
;; screen : main-screen?
;; 主屏幕驱动实例。
;;
;; 返回值
;; ----
;; #t
;; 操作成功返回 #t。

(import (scheme base)
  (liii check)
  (liii tui terminal)
  (liii tui main-screen buffer)
  (liii tui main-screen driver)
) ;import

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 10 :height 3))
       (screen (make-main-screen vt))
      ) ;
  (screen-render! screen '("hello"))
  (check (screen-buffer-dirty? (screen-buffer screen)) => #f)
  (check (screen-invalidate! screen) => #t)
  (check (screen-buffer-dirty? (screen-buffer screen)) => #t)
) ;let*

(check-catch 'type-error (screen-invalidate! "not-a-screen"))

(check-report)
