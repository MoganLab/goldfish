;; screen-buffer-prev-lines-set!
;; 直接设置屏幕行缓冲区中上一帧的文本行列表。
;;
;; 语法
;; ----
;; (screen-buffer-prev-lines-set! buf lines)
;;
;; 参数
;; ----
;; buf : screen-buffer?
;; 屏幕行缓冲区实例。
;;
;; lines : list?
;; 新的上一帧文本行列表。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui main-screen buffer))

(check-set-mode! 'report-failed)

(let ((buf (make-screen-buffer)))
  (screen-buffer-prev-lines-set! buf '("prev custom"))
  (check (screen-buffer-prev-lines buf) => '("prev custom"))
) ;let

(let ((buf (make-screen-buffer)))
  (check-catch 'type-error (screen-buffer-prev-lines-set! buf "not-a-list"))
) ;let

(check-report)
