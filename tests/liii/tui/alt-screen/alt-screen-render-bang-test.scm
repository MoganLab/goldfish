;; alt-screen-render!
;; 将文本行列表渲染到备用屏终端表面，支持增量重绘与硬件光标定位。
;;
;; 语法
;; ----
;; (alt-screen-render! screen lines [cursor])
;;
;; 参数
;; ----
;; screen : alt-screen?
;; 备用屏驱动实例。
;;
;; lines : list?
;; 待渲染的文本行字符串列表。
;;
;; cursor : pair? (可选)
;; 光标位置点对 (col . row)（0-based）。
;;
;; 返回值
;; ----
;; boolean?
;; 渲染完成返回 #t。

(import (scheme base) (liii check) (liii tui alt-screen) (liii tui terminal))

(check-set-mode! 'report-failed)

;; 1. 整帧渲染：行内容按绝对行号落位，未使用的行补空行
(let* ((vt (make-virtual-terminal :width 20 :height 3)) (screen (make-alt-screen vt)))
  (check (alt-screen-render! screen '("line1" "line2") '(0 . 0)) => #t)
  (let ((lines (virtual-terminal-screen vt)))
    (check (car lines) => "line1               ")
    (check (cadr lines) => "line2               ")
    (check (caddr lines) => "                    ")
  ) ;let
  (check (alt-screen-lines screen) => '("line1" "line2" ""))
  (check (terminal-cursor vt) => '(0 0))
) ;let*

;; 2. 增量重绘：仅更新变化行
(let* ((vt (make-virtual-terminal :width 20 :height 3)) (screen (make-alt-screen vt)))
  (alt-screen-render! screen '("a" "b" "c") '(0 . 0))
  (alt-screen-render! screen '("a" "X" "c") '(0 . 0))
  (let ((lines (virtual-terminal-screen vt)))
    (check (car lines) => "a                   ")
    (check (cadr lines) => "X                   ")
    (check (caddr lines) => "c                   ")
  ) ;let
) ;let*

;; 3. 光标定位与可见性
(let* ((vt (make-virtual-terminal :width 20 :height 5)) (screen (make-alt-screen vt)))
  (terminal-hide-cursor! vt)
  (alt-screen-render! screen '("hello") '(3 . 2))
  (check (terminal-cursor vt) => '(3 2))
  (check (terminal-cursor-visible? vt) => #f)
) ;let*

;; 4. 类型检查
(check-catch 'type-error (alt-screen-render! 123 '("a")))

(check-report)
