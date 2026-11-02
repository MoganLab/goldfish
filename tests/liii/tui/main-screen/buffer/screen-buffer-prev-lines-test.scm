;; screen-buffer-prev-lines
;; 获取屏幕行缓冲区中上一帧的文本行列表。
;;
;; 语法
;; ----
;; (screen-buffer-prev-lines buf)
;;
;; 参数
;; ----
;; buf : screen-buffer?
;; 屏幕行缓冲区实例。
;;
;; 返回值
;; ----
;; list?
;; 上一帧的文本行列表。

(import (scheme base) (liii check) (liii tui main-screen buffer))

(check-set-mode! 'report-failed)

(let ((buf (make-screen-buffer)))
  (check (screen-buffer-prev-lines buf) => '())
  (screen-buffer-commit! buf '("line 1" "line 2"))
  (check (screen-buffer-prev-lines buf) => '())
  (screen-buffer-commit! buf '("modified 1"))
  (check (screen-buffer-prev-lines buf) => '("line 1" "line 2"))
) ;let

(check-catch 'type-error (screen-buffer-prev-lines "not-a-buffer"))

(check-report)
