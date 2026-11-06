;; screen-buffer-clear!
;; 清空屏幕行缓冲区的当前帧内容并标记为脏状态。
;;
;; 语法
;; ----
;; (screen-buffer-clear! buf)
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
  (screen-buffer-clear! buf)
  (check (screen-buffer-lines buf) => '())
  (check (screen-buffer-prev-lines buf) => '("line 1"))
  (check-true (screen-buffer-dirty? buf))
) ;let

(check-catch 'type-error (screen-buffer-clear! "not-a-buffer"))

(check-report)
