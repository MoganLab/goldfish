;; alt-screen-invalidate!
;; 使备用屏的行缓存失效，强制下一次渲染进行全量重绘。
;;
;; 语法
;; ----
;; (alt-screen-invalidate! screen)
;;
;; 参数
;; ----
;; screen : alt-screen?
;; 备用屏驱动实例。
;;
;; 返回值
;; ----
;; boolean?
;; 操作成功返回 #t。

(import (scheme base) (liii check) (liii tui alt-screen) (liii tui terminal))

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 10 :height 3)) (screen (make-alt-screen vt)))
  (alt-screen-render! screen '("a" "b") '(0 . 0))
  (check-true (alt-screen-invalidate! screen))
) ;let*

(check-catch 'type-error (alt-screen-invalidate! "not-a-screen"))

(check-report)
