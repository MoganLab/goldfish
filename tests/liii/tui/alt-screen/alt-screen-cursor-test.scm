(import (liii check)
        (liii tui alt-screen)
        (liii tui terminal))

(check-set-mode! 'report-failed)

;; 光标定位与可见性（对齐上游 tui-alt-screen.ts）：
;; 硬件光标仅用于 IME 候选窗定位，定位后保持隐藏，
;; 软件光标（反显方块）由组件层负责绘制。

;; 传光标参数：定位到指定位置且保持隐藏
(let* ((vt (make-virtual-terminal :width 20 :height 5))
       (screen (make-alt-screen vt)))
  (terminal-hide-cursor! vt)
  (alt-screen-render! screen '("hello") '(3 . 2))
  (check (terminal-cursor vt) => '(3 2))
  (check (terminal-cursor-visible? vt) => #f)
) ;let*

;; 传光标参数且光标当前可见：不主动显示（定位但隐藏）
(let* ((vt (make-virtual-terminal :width 20 :height 5))
       (screen (make-alt-screen vt)))
  (terminal-show-cursor! vt)
  (alt-screen-render! screen '("hello") '(1 . 1))
  (check (terminal-cursor vt) => '(1 1))
  (check (terminal-cursor-visible? vt) => #f)
) ;let*

;; 不传光标参数：隐藏硬件光标（软件光标由帧内容决定）
(let* ((vt (make-virtual-terminal :width 20 :height 5))
       (screen (make-alt-screen vt)))
  (alt-screen-render! screen '("hello"))
  (check (terminal-cursor-visible? vt) => #f)
) ;let*

(check-report)
