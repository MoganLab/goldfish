;; screen-buffer-lines
;; 获取屏幕行缓冲区中当前帧的文本行列表。
;;
;; 语法
;; ----
;; (screen-buffer-lines buf)
;;
;; 参数
;; ----
;; buf : screen-buffer?
;; 屏幕行缓冲区实例。
;;
;; 返回值
;; ----
;; list?
;; 当前帧的文本行列表。

(import (scheme base) (liii check) (liii tui main-screen buffer))

(check-set-mode! 'report-failed)

(let ((buf (make-screen-buffer)))
  (check (screen-buffer-lines buf) => '())
  (screen-buffer-commit! buf '("line 1" "line 2"))
  (check (screen-buffer-lines buf) => '("line 1" "line 2"))
) ;let

(check-catch 'type-error (screen-buffer-lines "not-a-buffer"))

(check-report)
