;; alt-screen-resize-check!
;; 检测备用屏终端尺寸是否变化：变化时更新记录并使整屏缓存失效
;; （下一次渲染自动全量重绘）；首次检测仅记录基准尺寸，不算变化。
;;
;; 语法
;; ----
;; (alt-screen-resize-check! screen)
;;
;; 参数
;; ----
;; screen : alt-screen?
;; 备用屏驱动实例。
;;
;; 返回值
;; ----
;; boolean?
;; 检测到尺寸变化返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui alt-screen) (liii tui terminal))

(check-set-mode! 'report-failed)

;; 首次检测仅记录基准：#f；尺寸不变：#f
(let* ((vt (make-virtual-terminal :width 20 :height 5)) (screen (make-alt-screen vt)))
  (alt-screen-render! screen '("a" "b"))
  (check (alt-screen-resize-check! screen) => #f)
  (check (alt-screen-resize-check! screen) => #f)
) ;let*

;; 变宽：#t；再次检测：#f；缩回原尺寸：#t（任何变化都算）
(let* ((vt (make-virtual-terminal :width 20 :height 5)) (screen (make-alt-screen vt)))
  (alt-screen-render! screen '("a" "b"))
  (check (alt-screen-resize-check! screen) => #f)
  (virtual-terminal-resize! vt 30 5)
  (check (alt-screen-resize-check! screen) => #t)
  (check (alt-screen-resize-check! screen) => #f)
  (virtual-terminal-resize! vt 20 3)
  (check (alt-screen-resize-check! screen) => #t)
) ;let*

;; 尺寸变化后缓存已失效：下一次渲染全量输出所有行（而非 diff 跳过）
(let* ((vt (make-virtual-terminal :width 10 :height 3)) (screen (make-alt-screen vt)))
  (alt-screen-render! screen '("aaa" "bbb" "ccc"))
  (check (alt-screen-resize-check! screen) => #f)
  (virtual-terminal-resize! vt 6 3)
  (check-true (alt-screen-resize-check! screen))
  (alt-screen-render! screen '("xxx" "yyy" "zzz"))
  (check (virtual-terminal-screen vt) => '("xxx   " "yyy   " "zzz   "))
) ;let*

(check-catch 'type-error (alt-screen-resize-check! "not-a-screen"))

(check-report)
