;; alt-screen-refresh!
;; 清屏并按当前帧内容强制全量重绘备用屏。
;;
;; 语法
;; ----
;; (alt-screen-refresh! screen)
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
  (check-true (alt-screen-refresh! screen))
  (let ((lines (virtual-terminal-screen vt)))
    (check (car lines) => "a         ")
    (check (cadr lines) => "b         ")
  ) ;let
) ;let*

(check-catch 'type-error (alt-screen-refresh! "not-a-screen"))

(check-report)
