;; screen-buffer-invalidate!
;; 将屏幕行缓冲区标记为脏状态，强制下一次渲染进行全量重绘。
;;
;; 语法
;; ----
;; (screen-buffer-invalidate! buf)
;;
;; 参数
;; ----
;; buf : screen-buffer?
;; 屏幕行缓冲区实例。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui main-screen buffer))

(check-set-mode! 'report-failed)

(let ((buf (make-screen-buffer)))
  (screen-buffer-commit! buf '("line 1"))
  (check-false (screen-buffer-dirty? buf))
  (screen-buffer-invalidate! buf)
  (check-true (screen-buffer-dirty? buf))
) ;let

(check-catch 'type-error (screen-buffer-invalidate! "not-a-buffer"))

(check-report)
