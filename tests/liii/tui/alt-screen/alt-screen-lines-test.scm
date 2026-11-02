;; alt-screen-lines
;; 获取备用屏当前缓存的帧文本行列表。
;;
;; 语法
;; ----
;; (alt-screen-lines screen)
;;
;; 参数
;; ----
;; screen : alt-screen?
;; 备用屏驱动实例。
;;
;; 返回值
;; ----
;; list?
;; 当前帧的文本行字符串列表。

(import (scheme base) (liii check) (liii tui alt-screen) (liii tui terminal))

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 20 :height 3)) (screen (make-alt-screen vt)))
  (check (alt-screen-lines screen) => '())
  (alt-screen-render! screen '("line1" "line2"))
  (check (alt-screen-lines screen) => '("line1" "line2" ""))
) ;let*

(check-catch 'type-error (alt-screen-lines "not-a-screen"))

(check-report)
