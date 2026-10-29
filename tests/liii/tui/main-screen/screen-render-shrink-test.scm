(import (liii check)
  (liii string)
  (liii tui terminal)
  (liii tui main-screen driver)
)

(check-set-mode! 'report-failed)

;; 内容收缩超过终端高度时的重绘策略（对齐上游 tui-main-screen）：
;; inline 场景下内容行数超过终端高度后发生收缩，相对移动 + 局部清行
;; 无法可靠清理滚出视口的残留，必须触发清屏全量重绘。

;; 1. 基础收缩（内容在一屏内）：允许局部清行，不清屏
(let* ((vt (make-virtual-terminal :width 10 :height 10))
       (screen (make-main-screen vt :auto-hide-cursor #f))
      )
  (screen-render! screen '("a" "b" "c" "d" "e"))
  (let ((raw-before (length (virtual-terminal-raw vt))))
    (screen-render! screen '("a" "b"))
    ;; screen 缓冲收缩
    (check (screen-lines screen) => '("a" "b"))
    ;; 未触发清屏（局部 diff 即可）
    (check-true (not (member "\x1b[2J" (virtual-terminal-raw vt))))
    (check-true (> (length (virtual-terminal-raw vt)) raw-before))
  ) ;let
) ;let*

;; 2. 超屏收缩：内容超过终端高度后收缩，必须清屏全量重绘
(let* ((vt (make-virtual-terminal :width 10 :height 5))
       (screen (make-main-screen vt :auto-hide-cursor #f))
      )
  (screen-render! screen '("a" "b" "c" "d" "e" "f" "g" "h"))
  ;; 首帧 8 行溢出 5 行高视口，屏幕只保留尾部 5 行
  (screen-render! screen '("a" "b" "c"))
  (check (screen-lines screen) => '("a" "b" "c"))
  ;; 全量重绘后虚拟屏幕恰为收缩后内容，无残留行（局部 diff 清不干净）
  (check (virtual-terminal-screen vt)
    => '("a         " "b         " "c         " "          " "          ")
  )
) ;let*

(check-report)
