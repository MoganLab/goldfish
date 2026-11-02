;; screen-buffer-dirty?
;; 判断屏幕行缓冲区当前是否处于脏状态（需要全量重绘）。
;;
;; 语法
;; ----
;; (screen-buffer-dirty? buf)
;;
;; 参数
;; ----
;; buf : screen-buffer?
;; 屏幕行缓冲区实例。
;;
;; 返回值
;; ----
;; boolean?
;; 若缓冲区为脏状态返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui main-screen buffer))

(check-set-mode! 'report-failed)

(let ((buf (make-screen-buffer)))
  (check-true (screen-buffer-dirty? buf))
  (screen-buffer-dirty-set! buf #f)
  (check-false (screen-buffer-dirty? buf))
  (screen-buffer-commit! buf '("line 1"))
  (check-false (screen-buffer-dirty? buf))
  (screen-buffer-invalidate! buf)
  (check-true (screen-buffer-dirty? buf))
) ;let

(check-catch 'type-error (screen-buffer-dirty? "not-a-buffer"))

(check-report)
