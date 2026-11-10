;; alt-screen-clear!
;; 清空备用屏终端表面与内部帧行缓冲区。
;;
;; 语法
;; ----
;; (alt-screen-clear! screen)
;;
;; 参数
;; ----
;; screen : alt-screen?
;; 备用屏驱动实例。
;;
;; 返回值
;; ----
;; boolean?
;; 清屏成功返回 #t。

(import (scheme base) (liii check) (liii tui alt-screen) (liii tui terminal))

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 10 :height 3)) (screen (make-alt-screen vt)))
  (alt-screen-render! screen '("x" "y" "z") '(0 . 0))
  (check (alt-screen-clear! screen) => #t)
  (check (alt-screen-lines screen) => '())
  (let ((lines (virtual-terminal-screen vt)))
    (check (car lines) => "          ")
    (check (cadr lines) => "          ")
    (check (caddr lines) => "          ")
  ) ;let
) ;let*

(check-catch 'type-error (alt-screen-clear! "not-a-screen"))

(check-report)
