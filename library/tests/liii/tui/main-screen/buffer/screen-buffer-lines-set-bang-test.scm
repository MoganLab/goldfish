;; screen-buffer-lines-set!
;; 直接设置屏幕行缓冲区中当前帧的文本行列表。
;;
;; 语法
;; ----
;; (screen-buffer-lines-set! buf lines)
;;
;; 参数
;; ----
;; buf : screen-buffer?
;; 屏幕行缓冲区实例。
;;
;; lines : list?
;; 新的当前帧文本行列表。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui main-screen buffer))

(check-set-mode! 'report-failed)

(let ((buf (make-screen-buffer)))
  (screen-buffer-lines-set! buf '("custom"))
  (check (screen-buffer-lines buf) => '("custom"))
) ;let

(let ((buf (make-screen-buffer)))
  (check-catch 'type-error (screen-buffer-lines-set! buf "not-a-list"))
) ;let

(check-report)
